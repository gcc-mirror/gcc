enum java_opcode {
#define JAVAOP(NAME, CODE, KIND, TYPE, VALUE) OPCODE_##NAME = CODE,
#include "javaop.def"
#undef JAVAOP
LAST_AND_UNUSED_JAVA_OPCODE
};
