__attribute__((noinline, noipa)) static void do_nothing() {}
__attribute__((noinline, noipa)) static void effect_1() { do_nothing(); }
__attribute__((noinline, noipa)) static void effect_2() { do_nothing(); }
