//  PR c++/11895
//  This used to ICE in reshape_init.
//  testcase from fnf@ninemoons.com

    __attribute__((vector_size(16))) int a1 = { 100, 200, 300, 400 };
