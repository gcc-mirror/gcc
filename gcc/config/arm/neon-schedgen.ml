(* Emission of the core of the Cortex-A8 NEON scheduling description.
   Copyright (C) 2007, 2010 Free Software Foundation, Inc.
   Contributed by CodeSourcery.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.
*)

(* This scheduling description generator works as follows.
   - Each group of instructions has source and destination requirements
     specified.  The source requirements may be specified using
     Source (the stage at which all source operands not otherwise
     described are read), Source_m (the stage at which Rm operands are
     read), Source_n (likewise for Rn) and Source_d (likewise for Rd).
   - For each group of instructions the earliest stage where a source
     operand may be required is calculated.
   - Each group of instructions is selected in turn as a producer.
     The latencies between this group and every other group are then
     calculated, yielding up to four values for each combination:
	1. Producer -> consumer Rn latency
	2. Producer -> consumer Rm latency
	3. Producer -> consumer Rd (as a source) latency
	4. Producer -> consumer worst-case latency.
     Value 4 is calculated from the destination availability requirements
     of the consumer and the earliest source availability requirements
     of the producer.
   - The largest Value 4 calculated for the current producer is the
     worse-case latency, L, for that instruction group.  This value is written
     out in a define_insn_reservation for the producer group.
   - For each producer and consumer pair, the latencies calculated above
     are collated.  The average (of up to four values) is calculated and
     if this average is different from the worst-case latency, an
     unguarded define_bypass construction is issued for that pair.
     (For each pair only one define_bypass construction will be emitted,
     and at present we do not emit specific guards.)
*)

let find_with_result fn lst =
  let rec scan = function
      [] -> raise Not_found
    | l::ls -> 
      match fn l with
          Some result -> result
       | _ -> scan ls in
    scan lst

let n1 = 1 and n2 = 2 and n3 = 3 and n4 = 4 and n5 = 5 and n6 = 6
    and n7 = 7 and n8 = 8 and n9 = 9

type availability = Source of int
                  | Source_n of int
                  | Source_m of int
                  | Source_d of int
                  | Dest of int
		  | Dest_n_after of int * int

type guard = Guard_none | Guard_only_m | Guard_only_n | Guard_only_d

(* Reservation behaviors.  All but the last row here correspond to one
   pipeline each.  Each constructor will correspond to one
   define_reservation.  *)
type reservation =
  Mul | Mul_2cycle | Mul_4cycle
| Shift | Shift_2cycle
| ALU | ALU_2cycle
| Fmul | Fmul_2cycle
| Fadd | Fadd_2cycle
(* | VFP *)
| Permute of int
| Ls of int
| Fmul_then_fadd | Fmul_then_fadd_2

(* This table must be kept as short as possible by conflating
   entries with the same availability behavior.

   First components: instruction group names
   Second components: availability requirements, in the order in which
   they should appear in the comments in the .md file.
   Third components: reservation info
*)
let availability_table = [
  (* NEON integer ALU instructions.  *)
  (* vbit vbif vbsl vorr vbic vnot vcls vclz vcnt vadd vand vorr
     veor vbic vorn ddd qqq *)
  "neon_int_1", [Source n2; Dest n3], ALU;
  (* vadd vsub qqd vsub ddd qqq *)
  "neon_int_2", [Source_m n1; Source_n n2; Dest n3], ALU;
  (* vsum vneg dd qq vadd vsub qdd *)
  "neon_int_3", [Source n1; Dest n3], ALU;
  (* vabs vceqz vcgez vcbtz vclez vcltz vadh vradh vsbh vrsbh dqq *)
  (* vhadd vrhadd vqadd vtst ddd qqq *)
  "neon_int_4", [Source n2; Dest n4], ALU;
  (* vabd qdd vhsub vqsub vabd vceq vcge vcgt vmax vmin vfmx vfmn ddd ddd *)
  "neon_int_5", [Source_m n1; Source_n n2; Dest n4], ALU;
  (* vqneg vqabs dd qq *)
  "neon_vqneg_vqabs", [Source n1; Dest n4], ALU;
  (* vmov vmvn *)
  "neon_vmov", [Dest n3], ALU;
  (* vaba *)
  "neon_vaba", [Source_n n2; Source_m n1; Source_d n3; Dest n6], ALU;
  "neon_vaba_qqq",
    [Source_n n2; Source_m n1; Source_d n3; Dest_n_after (1, n6)], ALU_2cycle;
  (* vsma *)
  "neon_vsma", [Source_m n1; Source_d n3; Dest n6], ALU;

  (* NEON integer multiply instructions.  *)
  (* vmul, vqdmlh, vqrdmlh *)
  (* vmul, vqdmul, qdd 16/8 long 32/16 long *)
  "neon_mul_ddd_8_16_qdd_16_8_long_32_16_long", [Source n2; Dest n6], Mul;
  "neon_mul_qqq_8_16_32_ddd_32", [Source n2; Dest_n_after (1, n6)], Mul_2cycle;
  (* vmul, vqdmul again *)
  "neon_mul_qdd_64_32_long_qqd_16_ddd_32_scalar_64_32_long_scalar",
    [Source_n n2; Source_m n1; Dest_n_after (1, n6)], Mul_2cycle;
  (* vmla, vmls *)
  "neon_mla_ddd_8_16_qdd_16_8_long_32_16_long",
    [Source_n n2; Source_m n2; Source_d n3; Dest n6], Mul;
  "neon_mla_qqq_8_16",
    [Source_n n2; Source_m n2; Source_d n3; Dest_n_after (1, n6)], Mul_2cycle;
  "neon_mla_ddd_32_qqd_16_ddd_32_scalar_qdd_64_32_long_scalar_qdd_64_32_long",
    [Source_n n2; Source_m n1; Source_d n3; Dest_n_after (1, n6)], Mul_2cycle;
  "neon_mla_qqq_32_qqd_32_scalar",
    [Source_n n2; Source_m n1; Source_d n3; Dest_n_after (3, n6)], Mul_4cycle;
  (* vmul, vqdmulh, vqrdmulh *)
  (* vmul, vqdmul *)
  "neon_mul_ddd_16_scalar_32_16_long_scalar",
    [Source_n n2; Source_m n1; Dest n6], Mul;
  "neon_mul_qqd_32_scalar",
    [Source_n n2; Source_m n1; Dest_n_after (3, n6)], Mul_4cycle;
  (* vmla, vmls *)
  (* vmla, vmla, vqdmla, vqdmls *)
  "neon_mla_ddd_16_scalar_qdd_32_16_long_scalar",
    [Source_n n2; Source_m n1; Source_d n3; Dest n6], Mul;

  (* NEON integer shift instructions.  *)
  (* vshr/vshl immediate, vshr_narrow, vshl_vmvh, vsli_vsri_ddd *)
  "neon_shift_1", [Source n1; Dest n3], Shift;
  (* vqshl, vrshr immediate; vqshr, vqmov, vrshr, vqrshr narrow;
     vqshl_vrshl_vqrshl_ddd *)
  "neon_shift_2", [Source n1; Dest n4], Shift;
  (* vsli, vsri and vshl for qqq *)
  "neon_shift_3", [Source n1; Dest_n_after (1, n3)], Shift_2cycle;
  "neon_vshl_ddd", [Source n1; Dest n1], Shift;
  "neon_vqshl_vrshl_vqrshl_qqq", [Source n1; Dest_n_after (1, n4)],
    Shift_2cycle;
  "neon_vsra_vrsra", [Source_m n1; Source_d n3; Dest n6], Shift;

  (* NEON floating-point instructions.  *)
  (* vadd, vsub, vabd, vmul, vceq, vcge, vcgt, vcage, vcagt, vmax, vmin *)
  (* vabs, vneg, vceqz, vcgez, vcgtz, vclez, vcltz, vrecpe, vrsqrte, vcvt *)
  "neon_fp_vadd_ddd_vabs_dd", [Source n2; Dest n5], Fadd;
  "neon_fp_vadd_qqq_vabs_qq", [Source n2; Dest_n_after (1, n5)],
    Fadd_2cycle;
  (* vsum, fvmx, vfmn *)
  "neon_fp_vsum", [Source n1; Dest n5], Fadd;
  "neon_fp_vmul_ddd", [Source_n n2; Source_m n1; Dest n5], Fmul;
  "neon_fp_vmul_qqd", [Source_n n2; Source_m n1; Dest_n_after (1, n5)],
    Fmul_2cycle;
  (* vmla, vmls *)
  "neon_fp_vmla_ddd",
    [Source_n n2; Source_m n2; Source_d n3; Dest n9], Fmul_then_fadd;
  "neon_fp_vmla_qqq",
    [Source_n n2; Source_m n2; Source_d n3; Dest_n_after (1, n9)],
    Fmul_then_fadd_2;
  "neon_fp_vmla_ddd_scalar",
    [Source_n n2; Source_m n1; Source_d n3; Dest n9], Fmul_then_fadd;
  "neon_fp_vmla_qqq_scalar",
    [Source_n n2; Source_m n1; Source_d n3; Dest_n_after (1, n9)],
    Fmul_then_fadd_2;
  "neon_fp_vrecps_vrsqrts_ddd", [Source n2; Dest n9], Fmul_then_fadd;
  "neon_fp_vrecps_vrsqrts_qqq", [Source n2; Dest_n_after (1, n9)],
    Fmul_then_fadd_2;

  (* NEON byte permute instructions.  *)
  (* vmov; vtrn and vswp for dd; vzip for dd; vuzp for dd; vrev; vext for dd *)
  "neon_bp_simple", [Source n1; Dest n2], Permute 1;
  (* vswp for qq; vext for qqq; vtbl with {Dn} or {Dn, Dn1};
     similarly for vtbx *)
  "neon_bp_2cycle", [Source n1; Dest_n_after (1, n2)], Permute 2;
  (* all the rest *)
  "neon_bp_3cycle", [Source n1; Dest_n_after (2, n2)], Permute 3;

  (* NEON load/store instructions.  *)
  "neon_ldr", [Dest n1], Ls 1;
  "neon_str", [Source n1], Ls 1;
  "neon_vld1_1_2_regs", [Dest_n_after (1, n1)], Ls 2;
  "neon_vld1_3_4_regs", [Dest_n_after (2, n1)], Ls 3;
  "neon_vld2_2_regs_vld1_vld2_all_lanes", [Dest_n_after (1, n2)], Ls 2;
  "neon_vld2_4_regs", [Dest_n_after (2, n2)], Ls 3;
  "neon_vld3_vld4", [Dest_n_after (3, n2)], Ls 4;
  "neon_vst1_1_2_regs_vst2_2_regs", [Source n1], Ls 2;
  "neon_vst1_3_4_regs", [Source n1], Ls 3;
  "neon_vst2_4_regs_vst3_vst4", [Source n1], Ls 4;
  "neon_vst3_vst4", [Source n1], Ls 4;
  "neon_vld1_vld2_lane", [Source n1; Dest_n_after (2, n2)], Ls 3;
  "neon_vld3_vld4_lane", [Source n1; Dest_n_after (4, n2)], Ls 5;
  "neon_vst1_vst2_lane", [Source n1], Ls 2;
  "neon_vst3_vst4_lane", [Source n1], Ls 3;
  "neon_vld3_vld4_all_lanes", [Dest_n_after (1, n2)], Ls 3;

  (* NEON register transfer instructions.  *)
  "neon_mcr", [Dest n2], Permute 1;
  "neon_mcr_2_mcrr", [Dest n2], Permute 2;
  (* MRC instructions are in the .tpl file.  *)
]

(* Augment the tuples in the availability table with an extra component
   that describes the earliest stage where a source operand may be
   required.  (It is also possible that an entry in the table has no
   source requirements.)  *)
let calculate_sources =
  List.map (fun (name, avail, res) ->
              let earliest_stage =
                List.fold_left
                  (fun cur -> fun info ->
                     match info with
                       Source stage
                     | Source_n stage
                     | Source_m stage
                     | Source_d stage ->
                         (match cur with
                           None -> Some stage
                         | Some stage' when stage < stage' -> Some stage
                         | _ -> cur)
                     | _ -> cur) None avail
              in
                (name, avail, res, earliest_stage))

(* Find the stage, if any, at the end of which a group produces a result.  *)
let find_dest (attr, avail, _, _) =
  try
    find_with_result
      (fun av -> match av with
                   Dest st -> Some (Some st)
                 | Dest_n_after (after, st) -> Some (Some (after + st))
                 | _ -> None) avail
  with Not_found -> None

(* Find the worst-case latency between a producer and a consumer.  *)
let worst_case_latency producer (_, _, _, earliest_required) =
  let dest = find_dest producer in
    match earliest_required, dest with
      None, _ ->
        (* The consumer doesn't have any source requirements.  *)
        None
    | _, None ->
        (* The producer doesn't produce any results (e.g. a store insn).  *)
        None
    | Some consumed, Some produced -> Some (produced - consumed + 1)

(* Helper function for below.  *)
let latency_calc f producer (_, avail, _, _) =
  try
    let source_avail = find_with_result f avail in
      match find_dest producer with
        None ->
          (* The producer does not produce a result.  *)
          Some 0
      | Some produced ->
          let latency = produced - source_avail + 1 in
            (* Latencies below zero are raised to zero since we don't have
               delay slots.  *)
            if latency < 0 then Some 0 else Some latency
  with Not_found -> None

(* Find any Rm latency between a producer and a consumer.  If no
   Rm source requirement is explicitly specified for the consumer,
   return "positive infinity".  Also return "positive infinity" if
   the latency matches the supplied worst-case latency for this
   producer.  *)
let get_m_latency producer consumer =
  match latency_calc (fun av -> match av with Source_m stage -> Some stage
                                            | _ -> None) producer consumer
  with None -> [] | Some latency -> [(Guard_only_m, latency)]

(* Likewise for Rn.  *)
let get_n_latency producer consumer =
  match latency_calc (fun av -> match av with Source_n stage -> Some stage
                                            | _ -> None) producer consumer
  with None -> [] | Some latency -> [(Guard_only_n, latency)]

(* Likewise for Rd.  *)
let get_d_latency producer consumer =
  match
    latency_calc (fun av -> match av with Source_d stage -> Some stage
                                        | _ -> None) producer consumer
  with None -> [] | Some latency -> [(Guard_only_d, latency)]

(* Given a producer and a consumer, work out the latency of the producer
   to the consumer in each of the four cases (availability information
   permitting) identified at the top of this file.  Return the
   consumer, the worst-case unguarded latency and any guarded latencies.  *)
let calculate_latencies producer consumer =
  let worst = worst_case_latency producer consumer in
  let m_latency = get_m_latency producer consumer in
  let n_latency = get_n_latency producer consumer in
  let d_latency = get_d_latency producer consumer in
    (consumer, worst, m_latency @ n_latency @ d_latency)

(* Helper function for below.  *)
let pick_latency largest worst guards =
  let guards =
    match worst with
      None -> guards
    | Some worst -> (Guard_none, worst) :: guards
  in
  if List.length guards = 0 then None else
    let total_latency =
      List.fold_left (fun acc -> fun (_, latency) -> acc + latency) 0 guards
    in
    let average_latency = (float_of_int total_latency) /.
                          (float_of_int (List.length guards)) in
    let rounded_latency = int_of_float (ceil average_latency) in
      if rounded_latency = largest then None
      else Some (Guard_none, rounded_latency)

(* Collate all bypasses for a particular producer as required in
   worst_case_latencies_and_bypasses.  (By this stage there is a maximum
   of one bypass from this producer to any particular consumer listed
   in LATENCIES.)  Use a hash table to collate bypasses with the
   same latency and guard.  *)
let collate_bypasses (producer_name, _, _, _) largest latencies =
  let ht = Hashtbl.create 42 in
  let keys = ref [] in
    List.iter (
      fun ((consumer, _, _, _), worst, guards) ->
        (* Find out which latency to use.  Ignoring latencies that match
           the *overall* worst-case latency for this producer (which will
           be in define_insn_reservation), we have to examine:
	   1. the latency with no guard between this producer and this
              consumer; and
	   2. any guarded latency.  *)
        let guard_latency_opt = pick_latency largest worst guards in
          match guard_latency_opt with
            None -> ()
          | Some (guard, latency) ->
            begin
              (if (try ignore (Hashtbl.find ht (guard, latency)); false
                   with Not_found -> true) then
                 keys := (guard, latency) :: !keys);
              Hashtbl.add ht (guard, latency) consumer
            end
    ) latencies;
    (* The hash table now has bypasses collated so that ones with the
       same latency and guard have the same keys.  Walk through all the
       keys, extract the associated bypasses, and concatenate the names
       of the consumers for each bypass.  *)
    List.map (
      fun ((guard, latency) as key) ->
        let consumers = Hashtbl.find_all ht key in
          (producer_name,
           String.concat ",\\\n               " consumers,
           latency,
           guard)
      ) !keys

(* For every producer, find the worst-case latency between it and
   *any* consumer.  Also determine (if such a thing exists) the
   lowest-latency bypass from each producer to each consumer.  Group
   the output in such a way that all bypasses with the same producer
   and latency are together, and so that bypasses with the worst-case
   latency are ignored.  *)
let worst_case_latencies_and_bypasses =
  let rec f (worst_acc, bypasses_acc) prev xs =
    match xs with
      [] -> (worst_acc, bypasses_acc)
    | ((producer_name, producer_avail, res_string, _) as producer)::next ->
      (* For this particular producer, work out the latencies between
         it and every consumer.  *)
      let latencies =
        List.fold_left (fun acc -> fun consumer ->
                          (calculate_latencies producer consumer) :: acc)
                       [] (prev @ xs)
      in
        (* Now work out what the overall worst case latency was for this
           particular producer.  *)
        match latencies with
          [] -> assert false
        | _ ->
          let comp_fn (_, l1, _) (_, l2, _) =
            if l1 > l2 then -1 else if l1 = l2 then 0 else 1
          in
          let largest =
            match List.hd (List.sort comp_fn latencies) with
              (_, None, _) -> 0 (* Producer has no consumers. *)
            | (_, Some worst, _) -> worst
          in
          (* Having got the largest latency, collect all bypasses for
             this producer and filter out those with that larger
             latency.  Record the others for later emission.  *)
          let bypasses = collate_bypasses producer largest latencies in
            (* Go on to process remaining producers, having noted
               the result for this one.  *)
            f ((producer_name, producer_avail, largest,
                res_string) :: worst_acc,
               bypasses @ bypasses_acc)
              (prev @ [producer]) next
  in
    f ([], []) []

(* Emit a helpful comment for a define_insn_reservation.  *)
let write_comment producer avail =
  let seen_source = ref false in
  let describe info =
    let read = if !seen_source then "" else "read " in
    match info with
      Source stage ->
        seen_source := true;
	Printf.printf "%stheir source operands at N%d" read stage
    | Source_n stage ->
        seen_source := true;
	Printf.printf "%stheir (D|Q)n operands at N%d" read stage
    | Source_m stage ->
        seen_source := true;
	Printf.printf "%stheir (D|Q)m operands at N%d" read stage
    | Source_d stage ->
	Printf.printf "%stheir (D|Q)d operands at N%d" read stage
    | Dest stage ->
	Printf.printf "produce a result at N%d" stage
    | Dest_n_after (after, stage) ->
	Printf.printf "produce a result at N%d on cycle %d" stage (after + 1)
  in
    Printf.printf ";; Instructions using this reservation ";
    let rec f infos x =
      let sep = if x mod 2 = 1 then "" else "\n;;" in
      match infos with
        [] -> assert false
      | [info] -> describe info; Printf.printf ".\n"
      | info::(_::[] as infos) ->
          describe info; Printf.printf ", and%s " sep; f infos (x+1)
      | info::infos -> describe info; Printf.printf ",%s " sep; f infos (x+1)
    in
      f avail 0

(* Emit a define_insn_reservation for each producer.  The latency
   written in will be its worst-case latency.  *)
let emit_insn_reservations =
  List.iter (
     fun (producer, avail, latency, reservation) ->
        write_comment producer avail;
        Printf.printf "(define_insn_reservation \"%s\" %d\n" producer latency;
        Printf.printf "  (and (eq_attr \"tune\" \"cortexa8\")\n";
        Printf.printf "       (eq_attr \"neon_type\" \"%s\"))\n" producer;
        let str =
          match reservation with
	    Mul -> "dp" | Mul_2cycle -> "dp_2" | Mul_4cycle -> "dp_4"
	  | Shift -> "dp" | Shift_2cycle -> "dp_2"
	  | ALU -> "dp" | ALU_2cycle -> "dp_2"
	  | Fmul -> "dp" | Fmul_2cycle -> "dp_2"
	  | Fadd -> "fadd" | Fadd_2cycle -> "fadd_2"
	  | Ls 1 -> "ls"
          | Ls n -> "ls_" ^ (string_of_int n)
	  | Permute 1 -> "perm"
          | Permute n -> "perm_" ^ (string_of_int n)
	  | Fmul_then_fadd -> "fmul_then_fadd"
	  | Fmul_then_fadd_2 -> "fmul_then_fadd_2"
        in
          Printf.printf "  \"cortex_a8_neon_%s\")\n\n" str
    )

(* Given a guard description, return the name of the C function to
   be used as the guard for define_bypass.  *)
let guard_fn g =
  match g with
    Guard_only_m -> "arm_neon_only_m_dependency"
  | Guard_only_n -> "arm_neon_only_n_dependency"
  | Guard_only_d -> "arm_neon_only_d_dependency"
  | Guard_none -> assert false

(* Emit a define_bypass for each bypass.  *)
let emit_bypasses =
  List.iter (
      fun (producer, consumers, latency, guard) ->
        Printf.printf "(define_bypass %d \"%s\"\n" latency producer;
        if guard = Guard_none then
          Printf.printf "               \"%s\")\n\n" consumers
        else
          begin
            Printf.printf "               \"%s\"\n" consumers;
            Printf.printf "               \"%s\")\n\n" (guard_fn guard)
          end
    )

(* Program entry point.  *)
let main =
  let table = calculate_sources availability_table in
  let worst_cases, bypasses = worst_case_latencies_and_bypasses table in
    emit_insn_reservations (List.rev worst_cases);
    Printf.printf ";; Exceptions to the default latencies.\n\n";
    emit_bypasses bypasses

