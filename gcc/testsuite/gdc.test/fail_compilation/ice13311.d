/*
EXTRA_FILES: imports/a13311.d
TEST_OUTPUT:
---
fail_compilation/imports/a13311.d(8): Error: undefined identifier `PieceTree`
---
*/
module ice13311;

struct TextPiece
{
    import imports.a13311;
}
