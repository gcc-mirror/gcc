define pr
set debug_rtx ($)
end

document pr
Print the full structure of the rtx that is $.
Works only when an inferior is executing.
end

define prl
set debug_rtx_list ($, debug_rtx_count)
end

document prl
Print the full structure of all rtx insns beginning at $.
Works only when an inferior is executing.
Uses variable debug_rtx_count to control number of insns printed:
  debug_rtx_count > 0: print from $ on.
  debug_rtx_count < 0: print a window around $.

There is also debug_rtx_find (rtx, uid) that will scan a list for UID and print
it using debug_rtx_list. Usage example: set $foo=debug_rtx_find(first, 42)
end

define pt
set debug_tree ($)
end

document pt
Print the full structure of the tree that is $.
Works only when an inferior is executing.
end

define ptc
output (enum tree_code) $.common.code
echo \n
end

document ptc
Print the tree-code of the tree node that is $.
end

define pdn
output $.decl.name->identifier.pointer
echo \n
end

document pdn
Print the name of the decl-node that is $.
end

define ptn
output $.type.name->decl.name->identifier.pointer
echo \n
end

document ptn
Print the name of the type-node that is $.
end

define prc
output (enum rtx_code) $.code
echo \ (
output $.mode
echo )\n
end

document prc
Print the rtx-code and machine mode of the rtx that is $.
end

define pi
print $.fld[0].rtx@7
end

document pi
Print the fields of an instruction that is $.
end

define pbs
set print_binding_stack ()
end

document pbs
In cc1plus, print the current binding stack, frame by frame, up to and
including the global binding level.
end

# Don't let abort actually run, as it will make
# stdio stop working and therefore the `pr' command below as well.
b abort

# Make gdb complain about symbol reading errors.  This is so that gcc
# developers can see and fix bugs in gcc debug output.
set complaints 20
