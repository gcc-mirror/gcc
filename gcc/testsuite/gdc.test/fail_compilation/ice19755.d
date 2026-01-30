/* TEST_OUTPUT:
---
fail_compilation/ice19755.d(12): Error: no property `x` for `self` of type `ice19755.Thunk!int*`
fail_compilation/ice19755.d(8):        struct `Thunk` defined here
fail_compilation/ice19755.d(17): Error: template instance `ice19755.Thunk!int` error instantiating
---
*/
struct Thunk(Dummy) {
    void opAssign(int dlg) {}
    auto get() inout {
        Thunk* self;
        self.x = 0;
    }
    alias get this;
}

alias T = Thunk!int;
