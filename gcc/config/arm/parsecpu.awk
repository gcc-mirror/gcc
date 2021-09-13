# Manipulate the CPU, FPU and architecture descriptions for ARM.
# Copyright (C) 2017-2021 Free Software Foundation, Inc.
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

# Invoke this with '-v cmd=<cmd>"
# where <cmd> is one of:
#	data: Print the standard 'C' data tables for the CPUs
#	common-data: Print the 'C' data for shared driver/compiler files
#	native: Print the data structures used by the native driver
#	headers: Print the standard 'C' headers for the CPUs
#	isa: Generate the arm-isa.h header
#	md: Print the machine description fragment
#	opt: Print the option tables fragment
#	chkcpu <name>: Checks that <name> is a valid CPU
#	chktune <name>: Checks that <name> is a valid CPU
#	chkfpu <name>: Checks that <name> is a valid FPU
#	chkarch <name>: Checks that <arch> is a valid architecture

function fatal (m) {
    print "error ("lineno"): " m > "/dev/stderr"
    fatal_err = 1
    if (parse_done) exit 1
}

function toplevel () {
    if (cpu_name != "") fatal("missing \"end cpu\"")
    if (arch_name != "") fatal("missing \"end arch\"")
    if (fpu_name != "") fatal("missing \"end fpu\"")
}

function boilerplate (style) {
    ce = ""
    if (style == "C" ) {
	cs = "/* "
	cc = "   "
	ce = "  */"
    } else if (style == "md") {
	cc = "; "
	cs = cc
    } else if (style == "sh") {
	cc = "# "
	cs = cc
    } else fatal("Unknown comment style: "style)

    print cs "-*- buffer-read-only: t -*-"

    print cc "Generated automatically by parsecpu.awk from arm-cpus.in."
    print cc "Do not edit."
    print ""
    print cc "Copyright (C) 2011-2021 Free Software Foundation, Inc."
    print ""
    print cc "This file is part of GCC."
    print ""
    print cc "GCC is free software; you can redistribute it and/or modify"
    print cc "it under the terms of the GNU General Public License as"
    print cc "published by the Free Software Foundation; either version 3,"
    print cc "or (at your option) any later version."
    print ""
    print cc "GCC is distributed in the hope that it will be useful,"
    print cc "but WITHOUT ANY WARRANTY; without even the implied warranty of"
    print cc "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
    print cc "GNU General Public License for more details."
    print ""
    print cc "You should have received a copy of the GNU General Public"
    print cc "License along with GCC; see the file COPYING3.  If not see"
    print cc "<http://www.gnu.org/licenses/>." ce
    print ""
}

function tune_flag_pfx (f) {
    return "TF_" f
}

# Print out the bits for the features in FLIST, which may be a
# mixture of fgroup and individual bits.  Print each feature needed
# exactly once.  Terminate the list with isa_nobit.  Prefix each line by
# INDENT.  Does not print a new line at the end.
function print_isa_bits_for (flist, indent) {
    nbits = split (flist, bits)

    for (bit = 1; bit <= nbits; bit++) {
	if (bits[bit] in features) {
	    pbit[bits[bit]] = 1
	} else if (bits[bit] in fgroup) {
	    for (gbits in fgrp_bits) {
		split (gbits, bitsep, SUBSEP)
		if (bitsep[1] == bits[bit]) {
		    pbit[bitsep[2]] = 1
		}
	    }
	} else fatal("feature " bits[bit] " not declared")
    }
    zbit = ORS
    ORS = ""
    print indent "{\n" indent "  "
    ORS = ", "
    count = 0
    for (bname in pbit) {
	print "isa_bit_" bname
	count++
	if (count == 4) {
	    count = 0
	    ORS = ""
	    print "\n" indent "  "
	    ORS = ", "
	}
    }
    ORS = ""
    print "isa_nobit\n" indent "}"
    ORS = zbit
    delete pbit
}

function gen_headers () {
    boilerplate("C")

    print "enum processor_type"
    print "{"

    ncpus = split (cpu_list, cpus)

    for (n = 1; n <= ncpus; n++) {
	print "  TARGET_CPU_"cpu_cnames[cpus[n]]","
    }
    print "  TARGET_CPU_arm_none"
    print "};\n"

    print "enum arch_type"
    print "{"

    narchs = split (arch_list, archs)

    for (n = 1; n <= narchs; n++) {
	print "  TARGET_ARCH_"arch_cnames[archs[n]]","
    }
    print "  TARGET_ARCH_arm_none"
    print "};\n"

    print "enum fpu_type"
    print "{"

    nfpus = split (fpu_list, fpus)

    for (n = 1; n <= nfpus; n++) {
	print "  TARGET_FPU_"fpu_cnames[fpus[n]]","
    }
    print "  TARGET_FPU_auto"
    print "};"
}

function gen_isa () {
    boilerplate("C")
    print "enum isa_feature {"
    print "  isa_nobit = 0,"
    for (fbit in features) {
	print "  isa_bit_" fbit ","
    }
    print "  isa_num_bits"
    print "};\n"

    for (fgrp in fgroup) {
	print "#define ISA_"fgrp " \\"
	z = ORS
	ORS = ""
	first = 1
	for (bitcomb in fgrp_bits) {
	    split (bitcomb, bitsep, SUBSEP)
	    if (bitsep[1] == fgrp) {
		if (first) {
		    first = 0
		} else print ", \\\n"
		print "  isa_bit_" bitsep[2]
	    }
	}
	ORS = z
	print "\n"
    }

    print "struct fbit_implication {"
    print "  /* Represents a feature implication, where:"
    print "     ante IMPLIES cons"
    print "     meaning that if ante is enabled then we should"
    print "     also implicitly enable cons.  */"
    print "  enum isa_feature ante;"
    print "  enum isa_feature cons;"
    print "};\n"
    print "static const struct fbit_implication all_implied_fbits[] ="
    print "{"
    for (impl in implied_bits) {
      split (impl, impl_parts, SUBSEP)
      print "  { isa_bit_" impl_parts[2] ", isa_bit_" impl_parts[1] " },"
    }
    print "  { isa_nobit, isa_nobit }"
    print "};\n"
}

function gen_data () {
    boilerplate("C")

    print "static const cpu_tune all_tunes[] ="
    print "{"

    ncpus = split (cpu_list, cpus)

    for (n = 1; n <= ncpus; n++) {
	print "  { /* " cpus[n] ".  */"
	# scheduler
	if (cpus[n] in cpu_tune_for) {
	    if (! (cpu_tune_for[cpus[n]] in cpu_cnames)) {
		fatal("unknown \"tune for\" target " cpu_tune_for[cpus[n]] \
		      " for CPU " cpus[n])
	    }
	    print "    TARGET_CPU_" cpu_cnames[cpu_tune_for[cpus[n]]] ","
	} else {
	    print "    TARGET_CPU_" cpu_cnames[cpus[n]] ","
	}
	# tune_flags
	if (cpus[n] in cpu_tune_flags) {
	    print "    (" cpu_tune_flags[cpus[n]] "),"
	} else print "    0,"
	# tune
	print "    &arm_" cpu_cost[cpus[n]] "_tune"
	print "  },"
    }
    print "  {TARGET_CPU_arm_none, 0, NULL}"
    print "};"
}

function gen_comm_data () {
    boilerplate("C")

    ncpus = split (cpu_list, cpus)

    for (n = 1; n <= ncpus; n++) {
	if (cpus[n] in cpu_opts) {
	    print "static const cpu_arch_extension cpu_opttab_" \
		cpu_cnames[cpus[n]] "[] = {"
	    nopts = split (cpu_opts[cpus[n]], opts)
	    for (opt = 1; opt <= nopts; opt++) {
		print "  {"
		print "    \"" opts[opt] "\", " \
		    cpu_opt_remove[cpus[n],opts[opt]] ", false,"
		print_isa_bits_for(cpu_opt_isa[cpus[n],opts[opt]], "    ")
		print "\n  },"
	    }
	    if (cpus[n] in cpu_optaliases) {
		naliases = split (cpu_optaliases[cpus[n]], aliases)
		for (alias = 1; alias <= naliases; alias++) {
		    if (! ((cpus[n], \
			    cpu_opt_alias[cpus[n],aliases[alias]]) in \
			   cpu_opt_isa)) {
			fatal("Alias " aliases[alias] " target not defined " \
			      "for CPU " cpus[n])
		    }
		    equiv=cpu_opt_alias[cpus[n],aliases[alias]]
		    print "  {"
		    print "    \"" aliases[alias] "\", " \
			cpu_opt_remove[cpus[n],equiv] ", true, "
		    print_isa_bits_for(cpu_opt_isa[cpus[n],equiv], "    ")
		    print "\n  },"
		}
	    }
	    print "  { NULL, false, false, {isa_nobit}}"
	    print "};\n"
	}

	if (cpus[n] in cpu_aliases) {
	    print "static const cpu_alias cpu_aliastab_" \
		cpu_cnames[cpus[n]] "[] = {"
	    naliases = split (cpu_aliases[cpus[n]], aliases)
	    for (alias = 1; alias <= naliases; alias++) {
		print "  { \"" aliases[alias] "\", " \
		    cpu_alias_visible[cpus[n],aliases[alias]] "},"
	    }
	    print "  { NULL, false}"
	    print "};\n"
	}
    }

    print "const cpu_option all_cores[] ="
    print "{"

    for (n = 1; n <= ncpus; n++) {
	print "  {"
	print "    {"
	# common.name
	print "      \"" cpus[n] "\","
	# common.extensions
	if (cpus[n] in cpu_opts) {
	    print "      cpu_opttab_" cpu_cnames[cpus[n]] ","
	} else print "      NULL,"
	# common.isa_bits
	nfeats = split (cpu_arch[cpus[n]], feats, "+")
	if (! (feats[1] in arch_isa)) {
	    fatal("unknown arch " feats[1] " for cpu " cpus[n])
	}
	all_isa_bits = arch_isa[feats[1]]
	for (m = 2; m <= nfeats; m++) {
	    if (! ((feats[1], feats[m]) in arch_opt_isa)) {
		fatal("unknown feature " feats[m] " for architecture " feats[1])
	    }
	    if (arch_opt_remove[feats[1],feats[m]] == "true") {
		fatal("cannot remove features from architecture specs")
	    }
	    all_isa_bits = all_isa_bits " " arch_opt_isa[feats[1],feats[m]]
	}
	if (cpus[n] in cpu_isa) {
	    all_isa_bits = all_isa_bits " " cpu_isa[cpus[n]]
	}
	print_isa_bits_for(all_isa_bits, "      ")
	print "\n    },"
	# aliases
	if (cpus[n] in cpu_aliases) {
	    print "    cpu_aliastab_" cpu_cnames[cpus[n]] ","
	} else print "    NULL,"
	# arch
	print "    TARGET_ARCH_" arch_cnames[feats[1]]
	print "  },"
    }

    print "  {{NULL, NULL, {isa_nobit}}, NULL, TARGET_ARCH_arm_none}"
    print "};"

    narchs = split (arch_list, archs)

    for (n = 1; n <= narchs; n++) {
	if (archs[n] in arch_opts) {
	    print "static const struct cpu_arch_extension arch_opttab_" \
		arch_cnames[archs[n]] "[] = {"
	    nopts = split (arch_opts[archs[n]], opts)
	    for (opt = 1; opt <= nopts; opt++) {
		print "  {"
		print "    \"" opts[opt] "\", " \
		    arch_opt_remove[archs[n],opts[opt]] ", false,"
		print_isa_bits_for(arch_opt_isa[archs[n],opts[opt]], "    ")
		print "\n  },"
	    }
	    if (archs[n] in arch_optaliases) {
		naliases = split (arch_optaliases[archs[n]], aliases)
		for (alias = 1; alias <= naliases; alias++) {
		    if (! ((archs[n], \
			    arch_opt_alias[archs[n],aliases[alias]]) in \
			   arch_opt_isa)) {
			fatal("Alias " aliases[alias] " target not defined " \
			      "for architecture " archs[n])
		    }
		    equiv=arch_opt_alias[archs[n],aliases[alias]]
		    print "  {"
		    print "    \"" aliases[alias] "\", " \
			arch_opt_remove[archs[n],equiv] ", true, "
		    print_isa_bits_for(arch_opt_isa[archs[n],equiv], "    ")
		    print "\n  },"
		}
	    }
	    print "  { NULL, false, false, {isa_nobit}}"
	    print "};\n"
	} else if (archs[n] in arch_optaliases) {
	    fatal("Architecture " archs[n] " has option aliases but no options")
	}
    }

    print "const arch_option all_architectures[] ="
    print "{"

    for (n = 1; n <= narchs; n++) {
	print "  {"
	if (! (arch_tune_for[archs[n]] in cpu_cnames)) {
	    fatal("unknown \"tune for\" target " arch_tune_for[archs[n]] \
		  " for architecture " archs[n])
	}
	# common.name
	print "    \"" archs[n] "\","
	# common.extensions
	if (archs[n] in arch_opts) {
	    print "    arch_opttab_" arch_cnames[archs[n]] ","
	} else print "    NULL,"
	# common.isa_bits
	print_isa_bits_for(arch_isa[archs[n]], "    ")
	print ","
	# arch, base_arch
	print "    \"" arch_base[archs[n]] "\", BASE_ARCH_" \
	    arch_base[archs[n]] ","
	# profile letter code, or zero if none.
	if (archs[n] in arch_prof) {
	    print "    '" arch_prof[archs[n]] "',"
	} else {
	    print "    0,"
	}
	# tune_id
	print "    TARGET_CPU_" cpu_cnames[arch_tune_for[archs[n]]] ","
	print "  },"
    }

    print "  {{NULL, NULL, {isa_nobit}},"
    print "   NULL, BASE_ARCH_0, 0, TARGET_CPU_arm_none}"
    print "};\n"

    print "const arm_fpu_desc all_fpus[] ="
    print "{"

    nfpus = split (fpu_list, fpus)

    for (n = 1; n <= nfpus; n++) {
	print "  {"
	print "    \"" fpus[n] "\","
	print_isa_bits_for(fpu_isa[fpus[n]], "    ")
	print "\n  },"
    }

    print "};"
}

function gen_native () {
    boilerplate("C")

    for (vendor in vendor_ids) {
	print "static struct vendor_cpu vendor"vendor"_cpu_table[] = {"
	ncpus = split (cpu_list, cpus)

	for (n = 1; n <= ncpus; n++) {
	    if ((cpus[n] in cpu_vendor) && (cpus[n] in cpu_part)	\
		&& cpu_vendor[cpus[n]] == vendor) {
		print "  {\"0x"cpu_part[cpus[n]]"\", \""cpu_arch[cpus[n]]"\", \""cpus[n]"\"},"
	    }
	}
	print "  {NULL, NULL, NULL}"
	print "};"
    }

    print "\nstatic struct vendor vendors_table[] = {"
    for (vendor in vendor_ids) {
	print "  {\"0x"vendor"\", vendor"vendor"_cpu_table},"
    }
    print "  {NULL, NULL}"
    print "};"
}

function gen_md () {
    boilerplate("md")

    z = ORS
    ORS = ""
    print "(define_attr \"tune\"\n\t\""

    ncpus = split (cpu_list, cpus)

    for (n = 1; n < ncpus; n++) {
	if ((n % 3) != 0) {
	    ORS = ","
	} else ORS = ",\n\t"
	print cpu_cnames[cpus[n]]
    }
    ORS = z
    print cpu_cnames[cpus[ncpus]]"\""
    print "\t(const (symbol_ref \"((enum attr_tune) arm_tune)\")))"
}

function gen_opt () {
    boilerplate("md")

    print "Enum"
    print "Name(processor_type) Type(enum processor_type)"
    print "Known ARM CPUs (for use with the -mcpu= and -mtune= options):\n"

    ncpus = split (cpu_list, cpus)

    for (n = 1; n <= ncpus; n++) {
	print "EnumValue"
	print "Enum(processor_type) String(" cpus[n] \
	    ") Value( TARGET_CPU_"cpu_cnames[cpus[n]]")"
	print ""
    }

    print "Enum"
    print "Name(arm_arch) Type(int)"
    print "Known ARM architectures (for use with the -march= option):\n"

    narchs = split (arch_list, archs)

    for (n = 1; n <= narchs; n++) {
	print "EnumValue"
	print "Enum(arm_arch) String(" archs[n] \
	    ") Value("n - 1")"
	print ""
    }

    print "Enum"
    print "Name(arm_fpu) Type(enum fpu_type)"
    print "Known ARM FPUs (for use with the -mfpu= option):\n"

    nfpus = split (fpu_list, fpus)

    for (n = 1; n <= nfpus; n++) {
	print "EnumValue"
	print "Enum(arm_fpu) String(" fpus[n] \
	    ") Value(TARGET_FPU_"fpu_cnames[fpus[n]]")"
	print ""
    }

    print "EnumValue"
    print "Enum(arm_fpu) String(auto) Value(TARGET_FPU_auto)"
}

function check_cpu (name) {
    exts = split (name, extensions, "+")

    cpu_name = extensions[1]
    if (! (cpu_name in cpu_cnames)) {
	if (! (cpu_name in cpu_all_aliases)) {
	    return "error"
	}
	cpu_name = cpu_all_aliases[cpu_name]
    }

    for (n = 2; n <= exts; n++) {
	if (!((cpu_name, extensions[n]) in cpu_opt_remove)	\
	    && !((cpu_name, extensions[n]) in cpu_optaliases)) {
	    return "error"
	}
    }
    return name
}

function check_fpu (name) {
    if (! (name in fpu_cnames)) {
	return "error"
    }
    return fpu_cnames[name]
}

function check_arch (name) {
    exts = split (name, extensions, "+")

    if (! (extensions[1] in arch_isa)) {
	return "error"
    }

    for (n = 2; n <= exts; n++) {
	if (!((extensions[1], extensions[n]) in arch_opt_remove)	\
	    && !((extensions[1], extensions[n]) in arch_optaliases)) {
	    return "error"
	}
    }
    return name
}

BEGIN {
    cpu_name = ""
    arch_name = ""
    fpu_name = ""
    lineno = 0
    fatal_err = 0
    parse_done = 0
    if (cmd == "") fatal("Usage parsecpu.awk -v cmd=<xyz>")
}

# New line.  Reset parse status and increment line count for error messages
// {
    lineno++
    parse_ok = 0
}

# Comments must be on a line on their own.
/^#/ {
    parse_ok = 1
}

/^define feature / {
    if (NF != 3) fatal("syntax: define feature <name>")
    toplevel()
    fbit = $3
    if (fbit in features) fatal("feature " fbit " already defined")
    features[fbit] = 1
    parse_ok = 1
}

/^define fgroup / {
    if (NF < 4) fatal("syntax: define fgroup <name> <feature> [<feature>]*")
    toplevel()
    fgrp = $3
    if (fgrp in fgroup) fatal("feature group " fgrp " already defined")
    if (fgrp in features) fatal("feature group " fgrp " aliases a feature")
    fcount = NF
    for (n = 4; n <= fcount; n++) {
	feat = $n
	if (feat in features) {
	    fgrp_bits[fgrp,feat] = 1
	} else if (feat in fgroup) {
	    # fgroups may reference other fgroups, copy their bits
	    # to our bits.  To avoid recursion we don't set fgroup[fgrp]
	    # until after we have done this, so such attempts will result
	    # in an invalid group definition.
	    for (bitcomb in fgrp_bits) {
		split (bitcomb, bitsep, SUBSEP)
		if (bitsep[1] == feat) {
		    fgrp_bits[fgrp,bitsep[2]] = 1
		}
	    }
	} else fatal("feature group member " feat " unrecognized")
    }
    fgroup[fgrp] = 1
    parse_ok = 1
}

/^define implied / {
  if (NF < 4) fatal("syntax: define implied <name> [<feature-or-fgroup>]+\n" \
		    "Implied bits must be defined with at least one antecedent.")
  toplevel()
  fbit = $3
  if (fbit in features) fatal("implied feature " fbit " aliases a real feature")
  if (fbit in fgroup) fatal("implied feature " fbit " aliases a feature group")
  fcount = NF
  features[fbit] = 1
  for (n = 4; n <= fcount; n++) {
    ante = $n
    if (fbit == ante) fatal("feature cannot imply itself")
    else if (ante in features) {
      for (impl in implied_bits) {
	split(impl, impl_sep, SUBSEP)
	if (ante == impl_sep[1])
	  fatal(ante " implies implied bit " fbit		\
		". Chained implications not currently supported")
      }
      implied_bits[fbit, ante] = 1
    } else if (ante in fgroup) {
      for (bitcomb in fgrp_bits) {
	split(bitcomb, bitsep, SUBSEP)
	if (bitsep[1] == ante) {
	  implied_bits[fbit, bitsep[2]] = 1
	}
      }
    } else {
      fatal("implied bit antecedent " ante " unrecognized")
    }
  }
  parse_ok = 1
}

/^begin fpu / {
    if (NF != 3) fatal("syntax: begin fpu <name>")
    toplevel()
    fpu_name = $3
    parse_ok = 1
}

/^end fpu / {
    if (NF != 3) fatal("syntax: end fpu <name>")
    if (fpu_name != $3) fatal("mimatched end fpu")
    if (! (fpu_name in fpu_isa)) {
	fatal("fpu definition \"" fpu_name "\" lacks an \"isa\" statement")
    }
    fpu_cnames[fpu_name] = fpu_name
    gsub(/[-+.]/, "_", fpu_cnames[fpu_name])
    fpu_list = fpu_list " " fpu_name
    fpu_name = ""
    parse_ok = 1
}

/^begin arch / {
    if (NF != 3) fatal("syntax: begin arch <name>")
    toplevel()
    arch_name = $3
    parse_ok = 1
}

/^[ 	]*base / {
    if (NF != 2) fatal("syntax: base <architecture-base-name>")
    if (arch_name == "") fatal("\"base\" statement outside of arch block")
    arch_base[arch_name] = $2
    parse_ok = 1
}

/^[ 	]*profile / {
    if (NF != 2) fatal("syntax: profile <profile-name>")
    if (arch_name == "") fatal("\"profile\" statement outside of arch block")
    arch_prof[arch_name] = $2
    parse_ok = 1
}

/^end arch / {
    if (NF != 3) fatal("syntax: end arch <name>")
    if (arch_name != $3) fatal("mimatched end arch")
    if (! (arch_name in arch_tune_for)) {
	fatal("arch definition lacks a \"tune for\" statement")
    }
    if (! (arch_name in arch_isa)) {
	fatal("arch definition lacks an \"isa\" statement")
    }
    arch_list = arch_list " " arch_name
    arch_cnames[arch_name] = arch_name
    gsub(/[-+.]/, "_", arch_cnames[arch_name])
    arch_name = ""
    parse_ok = 1
}

/^begin cpu / {
    if (NF != 3) fatal("syntax: begin cpu <name>")
    toplevel()
    cpu_name = $3
    parse_ok = 1
    if (cpu_name in cpu_cnames) {
	fatal(cpu_name " is already defined")
    }
    if (cpu_name in cpu_all_aliases) {
	fatal(cpu_name " has already been defined as an alias")
    }
}

/^[ 	]*cname / {
    if (NF != 2) fatal("syntax: cname <identifier>")
    if (cpu_name == "") fatal("\"cname\" outside of cpu block")
    cpu_cnames[cpu_name] = $2
    parse_ok = 1
}

/^[ 	]*alias / {
    if (NF < 2) fatal("syntax: alias <name>+")
    if (cpu_name == "") fatal("\"alias\" outside of cpu block")
    alias_count = NF
    for (n = 2; n <= alias_count; n++) {
	visible = "true"
	alias = $n
	if (alias ~ /^!.*/) {
	    visible = "false"
	    gsub(/^!/, "", alias)
	}
	if (alias in cpu_cnames) {
	    fatal(alias " is already defined as a cpu name")
	}
	if (n == 2) {
	    cpu_aliases[cpu_name] = alias
	} else cpu_aliases[cpu_name] = cpu_aliases[cpu_name] " " alias
	cpu_alias_visible[cpu_name,alias] = visible
	if (alias in cpu_all_aliases) {
	    fatal(alias " is already an alias for " cpu_all_aliases[alias])
	}
	cpu_all_aliases[alias] = cpu_name
    }
    parse_ok = 1
}

/^[ 	]*tune for / {
    if (NF != 3) fatal("syntax: tune for <cpu-name>")
    if (cpu_name != "") {
	cpu_tune_for[cpu_name] = $3
    } else if (arch_name != "") {
	arch_tune_for[arch_name] = $3
    } else fatal("\"tune for\" outside of cpu or arch block")
    parse_ok = 1
}

/^[ 	]*tune flags / {
    if (NF < 3) fatal("syntax: tune flags <flag> [<flag>]*")
    flags=""
    flag_count = NF
    for (n = 3; n <= flag_count; n++) {
	if (n == 3) {
	    flags = tune_flag_pfx($n)
	} else flags = flags " | " tune_flag_pfx($n)
    }
    if (cpu_name != "") {
	cpu_tune_flags[cpu_name] = flags
    } else if (arch_name != "") {
	arch_tune_flags[arch_name] = flags
    } else fatal("\"tune flags\" outside of cpu or arch block")
    parse_ok = 1
}

/^[ 	]*architecture / {
    if (NF != 2) fatal("syntax: architecture <arch-name>")
    if (cpu_name == "") fatal("\"architecture\" outside of cpu block")
    cpu_arch[cpu_name] = $2
    parse_ok = 1
}

/^[ 	]*isa / {
    if (NF < 2) fatal("syntax: isa <feature-or-fgroup> [<feature-or-fgroup>]*")
    flags=""
    flag_count = NF
    for (n = 2; n <= flag_count; n++) {
	if (n == 2) {
	    flags = $n
	} else flags = flags " " $n
    }
    if (cpu_name != "") {
	cpu_isa[cpu_name] = flags
    } else if (arch_name != "") {
	arch_isa[arch_name] = flags
    } else  if (fpu_name != "") {
	fpu_isa[fpu_name] = flags
    } else fatal("\"isa\" outside of cpu, fpu or arch block")
    parse_ok = 1
}

/^[ 	]*option / {
    if (NF < 4) fatal("syntax: option <name> add|remove <feature-or-fgroup>+")
    name=$2
    if ($3 == "add") {
	remove = "false"
    } else if ($3 == "remove") {
	remove = "true"
    } else fatal("syntax: option <name> add|remove isa-list")
    flags=""
    flag_count = NF
    for (n = 4; n <= flag_count; n++) {
	if (n == 4) {
	    flags = $n
	} else flags = flags " " $n
    }
    if (cpu_name != "") {
	cpu_opts[cpu_name] = cpu_opts[cpu_name] " " name
	cpu_opt_remove[cpu_name,name] = remove
	cpu_opt_isa[cpu_name,name] = flags
    } else if (arch_name != "") {
	arch_opts[arch_name] = arch_opts[arch_name] " " name
	arch_opt_remove[arch_name,name] = remove
	arch_opt_isa[arch_name,name] = flags
    } else fatal("\"option\" outside of cpu or arch block")
    parse_ok = 1
}

/^[ 	]*optalias / {
    if (NF != 3) fatal("syntax: optalias <name> <option-name>")
    name=$2
    alias=$3
    if (cpu_name != "") {
	cpu_optaliases[cpu_name] = cpu_optaliases[cpu_name] " " name
	cpu_opt_alias[cpu_name,name] = alias
    } else if (arch_name != "") {
	arch_optaliases[arch_name] = arch_optaliases[arch_name] " " name
	arch_opt_alias[arch_name,name] = alias
    } else fatal("\"optalias\" outside of cpu or arch block")
    parse_ok = 1
}

/^[ 	]*costs / {
    if (NF != 2) fatal("syntax: costs <identifier>")
    if (cpu_name == "") fatal("\"costs\" outside of cpu block")
    cpu_cost[cpu_name] = $2
    parse_ok = 1
}

/^[ 	]*vendor / {
    if (NF != 2) fatal("syntax: vendor <vendor-id>")
    if (cpu_name == "") fatal("\"vendor\" outside of cpu block")
    cpu_vendor[cpu_name] = $2
    vendor_ids[$2] = 1
    parse_ok = 1
}

/^[ 	]*part / {
    if (NF < 2 || NF > 4) fatal("syntax: part <part-id> [minrev [maxrev]]")
    if (cpu_name == "") fatal("\"part\" outside of cpu block")
    cpu_part[cpu_name] = $2
    if (NF > 2) cpu_minrev[cpu_name] = $3
    if (NF == 4) cpu_maxrev[cpu_name] = $4
    parse_ok = 1
}

/^end cpu / {
    if (NF != 3) fatal("syntax: end cpu <name>")
    if (cpu_name != $3) fatal("mimatched end cpu")
    if (! (cpu_name in cpu_cnames)) {
	cpu_cnames[cpu_name] = cpu_name
	gsub(/[-+.]/, "_", cpu_cnames[cpu_name])
    }
    if (! (cpu_name in cpu_arch)) fatal("cpu definition lacks an architecture")
    if ((cpu_name in cpu_part) && !(cpu_name in cpu_vendor)) {
	fatal("part number specified for " cpu_name " but no vendor")
    }
    cpu_list = cpu_list " " cpu_name
    cpu_name = ""
    parse_ok = 1
}

/[^\s]/ {
    if (! parse_ok) fatal("Unrecognized statement: " $0)
}

END {
    parse_done = 1
    if (fatal_err) exit 1
    toplevel()
    if (cmd == "data") {
	gen_data()
    } else if (cmd == "common-data") {
	gen_comm_data()
    } else if (cmd == "native") {
	gen_native()
    } else if (cmd == "headers") {
	gen_headers()
    } else if (cmd == "isa") {
	gen_isa()
    } else if (cmd == "md") {
	gen_md()
    } else if (cmd == "opt") {
	gen_opt()
    } else if (cmd ~ /^chk(cpu|tune) /) {
	split (cmd, target)
	print check_cpu(target[2])
    } else if (cmd ~ /^chkarch /) {
	split (cmd, target)
	print check_arch(target[2])
    } else if (cmd ~ /^chkfpu /) {
	split (cmd, target)
	print check_fpu(target[2])
    } else fatal("unrecognized command: "cmd)
}
