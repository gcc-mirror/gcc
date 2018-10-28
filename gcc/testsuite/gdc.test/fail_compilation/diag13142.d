/*
TEST_OUTPUT:
---
fail_compilation/diag13142.d(25): Error: cannot implicitly convert expression `3` of type `int` to `TYPE`
---
*/

class Button
{
    enum TYPE // button type
    {
        COMMAND,
        CHECK,
        OPTION,
    }
}

class Toolbar
{
    enum ButtonTYPE // button type
    {
        COMMAND   = Button.TYPE.COMMAND,
        CHECK     = Button.TYPE.CHECK,
        OPTION    = Button.TYPE.OPTION,
        DELIMETER = Button.TYPE.max + 1
    }
}
