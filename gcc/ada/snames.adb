------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S N A M E S                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.6.10.1 $
--                                                                          --
--          Copyright (C) 1992-2001, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Namet; use Namet;

package body Snames is

   --  Table of names to be set by Initialize. Each name is terminated by a
   --  single #, and the end of the list is marked by a null entry, i.e. by
   --  two # marks in succession. Note that the table does not include the
   --  entries for a-z, since these are initialized by Namet itself.

   Preset_Names : constant String :=
     "_parent#" &
     "_tag#" &
     "off#" &
     "space#" &
     "time#" &
     "_init_proc#" &
     "_size#" &
     "_abort_signal#" &
     "_address_resolver#" &
     "_assign#" &
     "_chain#" &
     "_clean#" &
     "_controller#" &
     "_entry_bodies#" &
     "_expunge#" &
     "_final_list#" &
     "_idepth#" &
     "_init#" &
     "_local_final_list#" &
     "_master#" &
     "_object#" &
     "_priority#" &
     "_service#" &
     "_tags#" &
     "_task#" &
     "_task_id#" &
     "_task_info#" &
     "_task_name#" &
     "_trace_sp#" &
     "initialize#" &
     "adjust#" &
     "finalize#" &
     "next#" &
     "prev#" &
     "_deep_adjust#" &
     "_equality#" &
     "_deep_finalize#" &
     "_deep_initialize#" &
     "_input#" &
     "_output#" &
     "_ras_access#" &
     "_ras_dereference#" &
     "_read#" &
     "_rep_to_pos#" &
     "_write#" &
     "allocate#" &
     "deallocate#" &
     "dereference#" &
     "decimal_io#" &
     "enumeration_io#" &
     "fixed_io#" &
     "float_io#" &
     "integer_io#" &
     "modular_io#" &
     "a_textio#" &
     "a_witeio#" &
     "const#" &
     "<error>#" &
     "go#" &
     "put#" &
     "put_line#" &
     "to#" &
     "finalization#" &
     "finalization_root#" &
     "interfaces#" &
     "standard#" &
     "system#" &
     "text_io#" &
     "wide_text_io#" &
     "addr#" &
     "async#" &
     "get_active_partition_id#" &
     "get_rci_package_receiver#" &
     "origin#" &
     "params#" &
     "partition#" &
     "partition_interface#" &
     "ras#" &
     "rci_name#" &
     "receiver#" &
     "result#" &
     "rpc#" &
     "subp_id#" &
     "Oabs#" &
     "Oand#" &
     "Omod#" &
     "Onot#" &
     "Oor#" &
     "Orem#" &
     "Oxor#" &
     "Oeq#" &
     "One#" &
     "Olt#" &
     "Ole#" &
     "Ogt#" &
     "Oge#" &
     "Oadd#" &
     "Osubtract#" &
     "Oconcat#" &
     "Omultiply#" &
     "Odivide#" &
     "Oexpon#" &
     "ada_83#" &
     "ada_95#" &
     "c_pass_by_copy#" &
     "component_alignment#" &
     "discard_names#" &
     "elaboration_checks#" &
     "eliminate#" &
     "extend_system#" &
     "extensions_allowed#" &
     "external_name_casing#" &
     "float_representation#" &
     "initialize_scalars#" &
     "license#" &
     "locking_policy#" &
     "long_float#" &
     "no_run_time#" &
     "normalize_scalars#" &
     "polling#" &
     "propagate_exceptions#" &
     "queuing_policy#" &
     "ravenscar#" &
     "restricted_run_time#" &
     "restrictions#" &
     "reviewable#" &
     "source_file_name#" &
     "style_checks#" &
     "suppress#" &
     "task_dispatching_policy#" &
     "unsuppress#" &
     "use_vads_size#" &
     "warnings#" &
     "validity_checks#" &
     "abort_defer#" &
     "all_calls_remote#" &
     "annotate#" &
     "assert#" &
     "asynchronous#" &
     "atomic#" &
     "atomic_components#" &
     "attach_handler#" &
     "comment#" &
     "common_object#" &
     "complex_representation#" &
     "controlled#" &
     "convention#" &
     "cpp_class#" &
     "cpp_constructor#" &
     "cpp_virtual#" &
     "cpp_vtable#" &
     "debug#" &
     "elaborate#" &
     "elaborate_all#" &
     "elaborate_body#" &
     "export#" &
     "export_exception#" &
     "export_function#" &
     "export_object#" &
     "export_procedure#" &
     "export_valued_procedure#" &
     "external#" &
     "finalize_storage_only#" &
     "ident#" &
     "import#" &
     "import_exception#" &
     "import_function#" &
     "import_object#" &
     "import_procedure#" &
     "import_valued_procedure#" &
     "inline#" &
     "inline_always#" &
     "inline_generic#" &
     "inspection_point#" &
     "interface#" &
     "interface_name#" &
     "interrupt_handler#" &
     "interrupt_priority#" &
     "java_constructor#" &
     "java_interface#" &
     "link_with#" &
     "linker_alias#" &
     "linker_options#" &
     "linker_section#" &
     "list#" &
     "machine_attribute#" &
     "main#" &
     "main_storage#" &
     "memory_size#" &
     "no_return#" &
     "optimize#" &
     "pack#" &
     "page#" &
     "passive#" &
     "preelaborate#" &
     "priority#" &
     "psect_object#" &
     "pure#" &
     "pure_function#" &
     "remote_call_interface#" &
     "remote_types#" &
     "share_generic#" &
     "shared#" &
     "shared_passive#" &
     "source_reference#" &
     "stream_convert#" &
     "subtitle#" &
     "suppress_all#" &
     "suppress_debug_info#" &
     "suppress_initialization#" &
     "system_name#" &
     "task_info#" &
     "task_name#" &
     "task_storage#" &
     "time_slice#" &
     "title#" &
     "unchecked_union#" &
     "unimplemented_unit#" &
     "unreserve_all_interrupts#" &
     "volatile#" &
     "volatile_components#" &
     "weak_external#" &
     "ada#" &
     "asm#" &
     "assembler#" &
     "cobol#" &
     "cpp#" &
     "dll#" &
     "fortran#" &
     "intrinsic#" &
     "java#" &
     "stdcall#" &
     "stubbed#" &
     "win32#" &
     "as_is#" &
     "body_file_name#" &
     "casing#" &
     "code#" &
     "component#" &
     "component_size_4#" &
     "copy#" &
     "d_float#" &
     "descriptor#" &
     "default#" &
     "dot_replacement#" &
     "dynamic#" &
     "entity#" &
     "external_name#" &
     "first_optional_parameter#" &
     "form#" &
     "g_float#" &
     "gcc#" &
     "gnat#" &
     "gpl#" &
     "ieee_float#" &
     "internal#" &
     "link_name#" &
     "lowercase#" &
     "max_size#" &
     "mechanism#" &
     "mixedcase#" &
     "modified_gpl#" &
     "name#" &
     "nca#" &
     "no#" &
     "on#" &
     "parameter_types#" &
     "reference#" &
     "restricted#" &
     "result_mechanism#" &
     "result_type#" &
     "sb#" &
     "section#" &
     "semaphore#" &
     "spec_file_name#" &
     "static#" &
     "stack_size#" &
     "subunit_file_name#" &
     "task_stack_size_default#" &
     "task_type#" &
     "time_slicing_enabled#" &
     "top_guard#" &
     "uba#" &
     "ubs#" &
     "ubsb#" &
     "unit_name#" &
     "unknown#" &
     "unrestricted#" &
     "uppercase#" &
     "vax_float#" &
     "vms#" &
     "working_storage#" &
     "abort_signal#" &
     "access#" &
     "address#" &
     "address_size#" &
     "aft#" &
     "alignment#" &
     "asm_input#" &
     "asm_output#" &
     "ast_entry#" &
     "bit#" &
     "bit_order#" &
     "bit_position#" &
     "body_version#" &
     "callable#" &
     "caller#" &
     "code_address#" &
     "component_size#" &
     "compose#" &
     "constrained#" &
     "count#" &
     "default_bit_order#" &
     "definite#" &
     "delta#" &
     "denorm#" &
     "digits#" &
     "elaborated#" &
     "emax#" &
     "enum_rep#" &
     "epsilon#" &
     "exponent#" &
     "external_tag#" &
     "first#" &
     "first_bit#" &
     "fixed_value#" &
     "fore#" &
     "has_discriminants#" &
     "identity#" &
     "img#" &
     "integer_value#" &
     "large#" &
     "last#" &
     "last_bit#" &
     "leading_part#" &
     "length#" &
     "machine_emax#" &
     "machine_emin#" &
     "machine_mantissa#" &
     "machine_overflows#" &
     "machine_radix#" &
     "machine_rounds#" &
     "machine_size#" &
     "mantissa#" &
     "max_interrupt_priority#" &
     "max_priority#" &
     "max_size_in_storage_elements#" &
     "maximum_alignment#" &
     "mechanism_code#" &
     "model_emin#" &
     "model_epsilon#" &
     "model_mantissa#" &
     "model_small#" &
     "modulus#" &
     "null_parameter#" &
     "object_size#" &
     "partition_id#" &
     "passed_by_reference#" &
     "pos#" &
     "position#" &
     "range#" &
     "range_length#" &
     "round#" &
     "safe_emax#" &
     "safe_first#" &
     "safe_large#" &
     "safe_last#" &
     "safe_small#" &
     "scale#" &
     "scaling#" &
     "signed_zeros#" &
     "size#" &
     "small#" &
     "storage_size#" &
     "storage_unit#" &
     "tag#" &
     "terminated#" &
     "tick#" &
     "to_address#" &
     "type_class#" &
     "uet_address#" &
     "unbiased_rounding#" &
     "unchecked_access#" &
     "universal_literal_string#" &
     "unrestricted_access#" &
     "vads_size#" &
     "val#" &
     "valid#" &
     "value_size#" &
     "version#" &
     "wchar_t_size#" &
     "wide_width#" &
     "width#" &
     "word_size#" &
     "adjacent#" &
     "ceiling#" &
     "copy_sign#" &
     "floor#" &
     "fraction#" &
     "image#" &
     "input#" &
     "machine#" &
     "max#" &
     "min#" &
     "model#" &
     "pred#" &
     "remainder#" &
     "rounding#" &
     "succ#" &
     "truncation#" &
     "value#" &
     "wide_image#" &
     "wide_value#" &
     "output#" &
     "read#" &
     "write#" &
     "elab_body#" &
     "elab_spec#" &
     "storage_pool#" &
     "base#" &
     "class#" &
     "ceiling_locking#" &
     "inheritance_locking#" &
     "fifo_queuing#" &
     "priority_queuing#" &
     "fifo_within_priorities#" &
     "access_check#" &
     "accessibility_check#" &
     "discriminant_check#" &
     "division_check#" &
     "elaboration_check#" &
     "index_check#" &
     "length_check#" &
     "overflow_check#" &
     "range_check#" &
     "storage_check#" &
     "tag_check#" &
     "all_checks#" &
     "abort#" &
     "abs#" &
     "accept#" &
     "and#" &
     "all#" &
     "array#" &
     "at#" &
     "begin#" &
     "body#" &
     "case#" &
     "constant#" &
     "declare#" &
     "delay#" &
     "do#" &
     "else#" &
     "elsif#" &
     "end#" &
     "entry#" &
     "exception#" &
     "exit#" &
     "for#" &
     "function#" &
     "generic#" &
     "goto#" &
     "if#" &
     "in#" &
     "is#" &
     "limited#" &
     "loop#" &
     "mod#" &
     "new#" &
     "not#" &
     "null#" &
     "of#" &
     "or#" &
     "others#" &
     "out#" &
     "package#" &
     "pragma#" &
     "private#" &
     "procedure#" &
     "raise#" &
     "record#" &
     "rem#" &
     "renames#" &
     "return#" &
     "reverse#" &
     "select#" &
     "separate#" &
     "subtype#" &
     "task#" &
     "terminate#" &
     "then#" &
     "type#" &
     "use#" &
     "when#" &
     "while#" &
     "with#" &
     "xor#" &
     "divide#" &
     "enclosing_entity#" &
     "exception_information#" &
     "exception_message#" &
     "exception_name#" &
     "file#" &
     "import_address#" &
     "import_largest_value#" &
     "import_value#" &
     "is_negative#" &
     "line#" &
     "rotate_left#" &
     "rotate_right#" &
     "shift_left#" &
     "shift_right#" &
     "shift_right_arithmetic#" &
     "source_location#" &
     "unchecked_conversion#" &
     "unchecked_deallocation#" &
     "abstract#" &
     "aliased#" &
     "protected#" &
     "until#" &
     "requeue#" &
     "tagged#" &
     "raise_exception#" &
     "binder#" &
     "builder#" &
     "compiler#" &
     "cross_reference#" &
     "default_switches#" &
     "exec_dir#" &
     "extends#" &
     "finder#" &
     "gnatls#" &
     "gnatstub#" &
     "implementation#" &
     "implementation_exceptions#" &
     "implementation_suffix#" &
     "languages#" &
     "library_dir#" &
     "library_elaboration#" &
     "library_kind#" &
     "library_name#" &
     "library_version#" &
     "linker#" &
     "naming#" &
     "object_dir#" &
     "project#" &
     "separate_suffix#" &
     "source_dirs#" &
     "source_files#" &
     "source_list_file#" &
     "specification#" &
     "specification_exceptions#" &
     "specification_suffix#" &
     "switches#" &
      "#";

   ---------------------
   -- Generated Names --
   ---------------------

   --  This section lists the various cases of generated names which are
   --  built from existing names by adding unique leading and/or trailing
   --  upper case letters. In some cases these names are built recursively,
   --  in particular names built from types may be built from types which
   --  themselves have generated names. In this list, xxx represents an
   --  existing name to which identifying letters are prepended or appended,
   --  and a trailing n represents a serial number in an external name that
   --  has some semantic significance (e.g. the n'th index type of an array).

   --    xxxA    access type for formal xxx in entry param record   (Exp_Ch9)
   --    xxxB    tag table for tagged type xxx                      (Exp_Ch3)
   --    xxxB    task body procedure for task xxx                   (Exp_Ch9)
   --    xxxD    dispatch table for tagged type xxx                 (Exp_Ch3)
   --    xxxD    discriminal for discriminant xxx                   (Sem_Ch3)
   --    xxxDn   n'th discr check function for rec type xxx         (Exp_Ch3)
   --    xxxE    elaboration boolean flag for task xxx              (Exp_Ch9)
   --    xxxE    dispatch table pointer type for tagged type xxx    (Exp_Ch3)
   --    xxxE    parameters for accept body for entry xxx           (Exp_Ch9)
   --    xxxFn   n'th primitive of a tagged type (named xxx)        (Exp_Ch3)
   --    xxxI    initialization procedure for type xxx              (Exp_Ch3)
   --    xxxJ    tag table type index for tagged type xxx           (Exp_Ch3)
   --    xxxM    master Id value for access type xxx                (Exp_Ch3)
   --    xxxP    tag table pointer type for tagged type xxx         (Exp_Ch3)
   --    xxxP    parameter record type for entry xxx                (Exp_Ch9)
   --    xxxPA   access to parameter record type for entry xxx      (Exp_Ch9)
   --    xxxPn   pointer type for n'th primitive of tagged type xxx (Exp_Ch3)
   --    xxxR    dispatch table pointer for tagged type xxx         (Exp_Ch3)
   --    xxxT    tag table type for tagged type xxx                 (Exp_Ch3)
   --    xxxT    literal table for enumeration type xxx             (Sem_Ch3)
   --    xxxV    type for task value record for task xxx            (Exp_Ch9)
   --    xxxX    entry index constant                               (Exp_Ch9)
   --    xxxY    dispatch table type for tagged type xxx            (Exp_Ch3)
   --    xxxZ    size variable for task xxx                         (Exp_Ch9)

   --  Implicit type names

   --    TxxxT   type of literal table for enumeration type xxx     (Sem_Ch3)

   --  (list not yet complete ???)

   ----------------------
   -- Get_Attribute_Id --
   ----------------------

   function Get_Attribute_Id (N : Name_Id) return Attribute_Id is
   begin
      return Attribute_Id'Val (N - First_Attribute_Name);
   end Get_Attribute_Id;

   ------------------
   -- Get_Check_Id --
   ------------------

   function Get_Check_Id (N : Name_Id) return Check_Id is
   begin
      return Check_Id'Val (N - First_Check_Name);
   end Get_Check_Id;

   -----------------------
   -- Get_Convention_Id --
   -----------------------

   function Get_Convention_Id (N : Name_Id) return Convention_Id is
   begin
      case N is
         when Name_Ada        => return Convention_Ada;
         when Name_Asm        => return Convention_Assembler;
         when Name_Assembler  => return Convention_Assembler;
         when Name_C          => return Convention_C;
         when Name_COBOL      => return Convention_COBOL;
         when Name_CPP        => return Convention_CPP;
         when Name_DLL        => return Convention_Stdcall;
         when Name_Fortran    => return Convention_Fortran;
         when Name_Intrinsic  => return Convention_Intrinsic;
         when Name_Java       => return Convention_Java;
         when Name_Stdcall    => return Convention_Stdcall;
         when Name_Stubbed    => return Convention_Stubbed;
         when Name_Win32      => return Convention_Stdcall;
         when others          =>
            raise Program_Error;
      end case;
   end Get_Convention_Id;

   ---------------------------
   -- Get_Locking_Policy_Id --
   ---------------------------

   function Get_Locking_Policy_Id (N : Name_Id) return Locking_Policy_Id is
   begin
      return Locking_Policy_Id'Val (N - First_Locking_Policy_Name);
   end Get_Locking_Policy_Id;

   -------------------
   -- Get_Pragma_Id --
   -------------------

   function Get_Pragma_Id (N : Name_Id) return Pragma_Id is
   begin
      if N = Name_AST_Entry then
         return Pragma_AST_Entry;
      elsif N = Name_Storage_Size then
         return Pragma_Storage_Size;
      elsif N = Name_Storage_Unit then
         return Pragma_Storage_Unit;
      else
         return Pragma_Id'Val (N - First_Pragma_Name);
      end if;
   end Get_Pragma_Id;

   ---------------------------
   -- Get_Queuing_Policy_Id --
   ---------------------------

   function Get_Queuing_Policy_Id (N : Name_Id) return Queuing_Policy_Id is
   begin
      return Queuing_Policy_Id'Val (N - First_Queuing_Policy_Name);
   end Get_Queuing_Policy_Id;

   ------------------------------------
   -- Get_Task_Dispatching_Policy_Id --
   ------------------------------------

   function Get_Task_Dispatching_Policy_Id (N : Name_Id)
     return Task_Dispatching_Policy_Id is
   begin
      return Task_Dispatching_Policy_Id'Val
        (N - First_Task_Dispatching_Policy_Name);
   end Get_Task_Dispatching_Policy_Id;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      P_Index      : Natural;
      Discard_Name : Name_Id;

   begin
      P_Index := Preset_Names'First;

      loop
         Name_Len := 0;

         while Preset_Names (P_Index) /= '#' loop
            Name_Len := Name_Len + 1;
            Name_Buffer (Name_Len) := Preset_Names (P_Index);
            P_Index := P_Index + 1;
         end loop;

         --  We do the Name_Find call to enter the name into the table, but
         --  we don't need to do anything with the result, since we already
         --  initialized all the preset names to have the right value (we
         --  are depending on the order of the names and Preset_Names).

         Discard_Name := Name_Find;
         P_Index := P_Index + 1;
         exit when Preset_Names (P_Index) = '#';
      end loop;

      --  Make sure that number of names in standard table is correct. If
      --  this check fails, run utility program XSNAMES to construct a new
      --  properly matching version of the body.

      pragma Assert (Discard_Name = Last_Predefined_Name);
   end Initialize;

   -----------------------
   -- Is_Attribute_Name --
   -----------------------

   function Is_Attribute_Name (N : Name_Id) return Boolean is
   begin
      return N in First_Attribute_Name .. Last_Attribute_Name;
   end Is_Attribute_Name;

   -------------------
   -- Is_Check_Name --
   -------------------

   function Is_Check_Name (N : Name_Id) return Boolean is
   begin
      return N in First_Check_Name .. Last_Check_Name;
   end Is_Check_Name;

   ------------------------
   -- Is_Convention_Name --
   ------------------------

   function Is_Convention_Name (N : Name_Id) return Boolean is
   begin
      return N in First_Convention_Name .. Last_Convention_Name
        or else N = Name_C;
   end Is_Convention_Name;

   ------------------------------
   -- Is_Entity_Attribute_Name --
   ------------------------------

   function Is_Entity_Attribute_Name (N : Name_Id) return Boolean is
   begin
      return N in First_Entity_Attribute_Name .. Last_Entity_Attribute_Name;
   end Is_Entity_Attribute_Name;

   --------------------------------
   -- Is_Function_Attribute_Name --
   --------------------------------

   function Is_Function_Attribute_Name (N : Name_Id) return Boolean is
   begin
      return N in
        First_Renamable_Function_Attribute ..
          Last_Renamable_Function_Attribute;
   end Is_Function_Attribute_Name;

   ----------------------------
   -- Is_Locking_Policy_Name --
   ----------------------------

   function Is_Locking_Policy_Name (N : Name_Id) return Boolean is
   begin
      return N in First_Locking_Policy_Name .. Last_Locking_Policy_Name;
   end Is_Locking_Policy_Name;

   -----------------------------
   -- Is_Operator_Symbol_Name --
   -----------------------------

   function Is_Operator_Symbol_Name (N : Name_Id) return Boolean is
   begin
      return N in First_Operator_Name .. Last_Operator_Name;
   end Is_Operator_Symbol_Name;

   --------------------
   -- Is_Pragma_Name --
   --------------------

   function Is_Pragma_Name (N : Name_Id) return Boolean is
   begin
      return N in First_Pragma_Name .. Last_Pragma_Name
        or else N = Name_AST_Entry
        or else N = Name_Storage_Size
        or else N = Name_Storage_Unit;
   end Is_Pragma_Name;

   ---------------------------------
   -- Is_Procedure_Attribute_Name --
   ---------------------------------

   function Is_Procedure_Attribute_Name (N : Name_Id) return Boolean is
   begin
      return N in First_Procedure_Attribute .. Last_Procedure_Attribute;
   end Is_Procedure_Attribute_Name;

   ----------------------------
   -- Is_Queuing_Policy_Name --
   ----------------------------

   function Is_Queuing_Policy_Name (N : Name_Id) return Boolean is
   begin
      return N in First_Queuing_Policy_Name .. Last_Queuing_Policy_Name;
   end Is_Queuing_Policy_Name;

   -------------------------------------
   -- Is_Task_Dispatching_Policy_Name --
   -------------------------------------

   function Is_Task_Dispatching_Policy_Name (N : Name_Id) return Boolean is
   begin
      return N in First_Task_Dispatching_Policy_Name ..
                  Last_Task_Dispatching_Policy_Name;
   end Is_Task_Dispatching_Policy_Name;

   ----------------------------
   -- Is_Type_Attribute_Name --
   ----------------------------

   function Is_Type_Attribute_Name (N : Name_Id) return Boolean is
   begin
      return N in First_Type_Attribute_Name .. Last_Type_Attribute_Name;
   end Is_Type_Attribute_Name;

end Snames;
