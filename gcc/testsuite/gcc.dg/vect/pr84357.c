/* { dg-do compile } */
/* { dg-additional-options "-Wall" } */

#define COUNT 32

typedef struct s1 {
    unsigned char c;
} s1;

typedef struct s2
{
    char pad;
    s1 arr[COUNT];
} s2;

typedef struct s3 {
    s1 arr[COUNT];
} s3;

s2 * get_s2();
s3 * gActiveS3;
void foo()
{
    s3 * three = gActiveS3;
    s2 * two = get_s2();

    for (int i = 0; i < COUNT; i++)
    {
        two->arr[i].c = three->arr[i].c;
    }
}
