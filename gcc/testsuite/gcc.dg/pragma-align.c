/* Prove that HANDLE_SYSTEMV_PRAGMA alignment handling works somewhat. */

/* { dg-do compile { target i?86-*-linux* i?86-*-*bsd* i?86-*-sco3.2v5* } } */

struct {
        char one;
        long two;
} defaultalign;

#pragma pack(4)
struct {
        char one;
        long two;
} sixteen;

#pragma pack(1)
struct {
        char one;
        long two;
} two;

#pragma pack(2)
struct {
        char one;
        long two;
} three;

#pragma pack()
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

