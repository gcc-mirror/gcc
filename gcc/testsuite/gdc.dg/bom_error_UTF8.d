// { dg-do compile }
﻿module object; // { dg-error "character 0xfeff is not a valid token" }

extern(C):
int printf(const char *, ...);

int main()
{
    printf("hello world\n");
    return 0;
}
