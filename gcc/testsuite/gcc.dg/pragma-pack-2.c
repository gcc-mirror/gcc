/* On Solaris, #pragma pack should accept macro expansion.  */

/* { dg-do run { target *-*-solaris2.* } } */

extern void abort (void);

struct {
        char one;
        long two;
} defaultalign;

#define ALIGNHIGH 16

#pragma pack(ALIGNHIGH)
struct {
        char one;
        long two;
} sixteen;

#define ALIGN1(X) 1
#pragma pack(ALIGN1(4))
struct {
        char one;
        long two;
} two;

#define ALIGN2(X) X
#pragma pack(ALIGN2(2))
struct {
        char one;
        long two;
} three;

#define EMPTY
#pragma pack(EMPTY)
struct {
        char one;
        long two;
} resetalign;

main()
{
        if(sizeof(sixteen) < sizeof(defaultalign)) abort();
        if(sizeof(two) >= sizeof(defaultalign)) abort();
        if(sizeof(three) <= sizeof(two)) abort();
        if(sizeof(resetalign) != sizeof(defaultalign)) abort();
	return 0;
}
