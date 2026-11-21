/* Regression test for leak false positive (PR analyzer/127593).  */

#define NULL ((void *)0)

struct public_stuff {
    void * data;
};

struct internal_stuff {
    int bar;
    struct public_stuff pub;
    int foo;
};

struct public_stuff *
create_public()
{
    struct internal_stuff * internal = __builtin_malloc(sizeof(struct internal_stuff));
    internal->pub.data = NULL;
    return &internal->pub; /* { dg-bogus "leak" "PR analyzer/127593" } */
}

void
free_public(struct public_stuff * pubs)
{
    struct internal_stuff * internal = (struct internal_stuff *)((unsigned char*)pubs - __builtin_offsetof(struct internal_stuff, pub));
    __builtin_free(internal);
}
