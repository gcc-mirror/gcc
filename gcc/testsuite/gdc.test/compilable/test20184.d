// EXTRA_FILES: imports/h20184.h
import imports.h20184;

alias fun_signature = extern (C) int();
static assert(is(typeof(f20184) : fun_signature));
