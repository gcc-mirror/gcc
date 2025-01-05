/*
TEST_OUTPUT:
---
fail_compilation/empty_statement.d(11): Error: use `{ }` for an empty statement, not `;`
fail_compilation/empty_statement.d(12): Error: use `{ }` for an empty statement, not `;`
fail_compilation/empty_statement.d(13): Error: use `{ }` for an empty statement, not `;`
---
*/
void main()
{
    for (;;);
    if (0);
    while (0);
}
