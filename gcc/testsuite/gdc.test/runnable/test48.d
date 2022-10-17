// EXTRA_SOURCES: imports/test48a.d
// PERMUTE_ARGS:

import core.stdc.stdio;
import imports.test48a;

void main()
{
    S s;
    auto i = s.tupleof[0] + s.tupleof[1] + s.tupleof[2];
    printf("i = %d\n", i);
    assert(i == 6);

    auto t = s.tupleof;
    i = t[0] + t[1] + t[2];
    printf("i = %d\n", i);
    assert(i == 6);

    printf("a = %zd %zd %zd\n", S.tupleof.offsetof);
    auto o = S.tupleof.offsetof;
    assert(o[0] == 0);
    assert(o[1] == 4);
    assert(o[2] == 8);
    printf("a = %zd %zd %zd\n", S.tupleof[0].offsetof, S.tupleof[1].offsetof, S.tupleof[2].offsetof);
    assert(S.tupleof[0].offsetof == 0);
    assert(S.tupleof[1].offsetof == 4);
    assert(S.tupleof[2].offsetof == 8);

    auto offset0 = cast(void*)&s.tupleof[0] - cast(void*)&s;
    printf("offset0 = %td\n", offset0);
    assert(offset0 == 0);

    auto offset1 = cast(void*)&s.tupleof[1] - cast(void*)&s;
    printf("offset1 = %td\n", offset1);
    assert(offset1 == 4);

    auto offset2 = cast(void*)&s.tupleof[2] - cast(void*)&s;
    printf("offset2 = %td\n", offset2);
    assert(offset2 == 8);

    int[S.tupleof.offsetof[1]] t1;
    assert(t1.length == 4);
}
