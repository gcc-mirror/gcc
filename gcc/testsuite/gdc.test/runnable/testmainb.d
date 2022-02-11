/*
Test that -main does nothing when main is already defined

REQUIRED_ARGS: -main
RUN_OUTPUT:
---
Success
---
*/
extern(C) int printf(const char*, ...);

void main()
{
    printf("Success\n");
}
