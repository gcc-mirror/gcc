/+
REQUIRED_ARGS: -Icompilable/extra-files
EXTRA_FILES: extra-files/ucn_vars.i
+/

import ucn_vars;

alias M = __traits(allMembers, Vars);
enum expected = ["x²", "Ⅰ", "ↂ", "À", "Á", "Â", "whÿ", "aÄbсδe", "b〡𗘰〣e"];

static foreach(i; 0 .. M.length) {
    static assert(M[i] == expected[i]);
}
