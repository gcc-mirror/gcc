// https://issues.dlang.org/show_bug.cgi?id=2450
/*
TEST_OUTPUT:
---
fail_compilation/fail2450.d(20): Error: function expected before `()`, not `this.mixin Event!() clicked;` of type `void`
fail_compilation/fail2450.d(23): Error: function expected before `()`, not `b.mixin Event!() clicked;` of type `void`
---
*/

template Event()
{
	void opCall() { }
	void opAddAssign(int i) { }
}
class Button {
	mixin Event clicked;
	void func()
    {
		clicked.opCall(); // works
		this.clicked();   // works

		auto b = new Button();
		b.clicked();      // works
	}
}
