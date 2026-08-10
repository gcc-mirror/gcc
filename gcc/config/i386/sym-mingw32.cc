/* Prevent any name mangling to make sure that the linker
   will always find the symbol. */
extern "C" __attribute__ ((visibility ("hidden")))
char HOST_EXTRA_OBJS_SYMBOL asm ("HOST_EXTRA_OBJS_SYMBOL");
