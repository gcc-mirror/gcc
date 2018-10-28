/* { dg-options "-fsanitize=address" } */
/* { dg-do compile } */

module asantests;


/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=272

extern(C) void my_memcmp(const(void) *s1, const(void) *s2);

void bug(const(char)* p)
{
        my_memcmp(p, "__FILE__".ptr);
}
