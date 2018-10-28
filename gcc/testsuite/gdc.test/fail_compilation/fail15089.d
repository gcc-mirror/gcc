/*
TEST_OUTPUT:
---
fail_compilation/fail15089.d(10): Error: cannot implicitly convert expression `130` of type `int` to `byte`
---
*/

enum Pieces {Rook = 2} /* line 1 */
immutable int color = 0b10000000;
byte piece = Pieces.Rook ^ color;
