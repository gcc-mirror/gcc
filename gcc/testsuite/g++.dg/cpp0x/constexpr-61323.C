// PR c++/61323
// { dg-do compile { target c++11 } }

char* table1[10];
template<unsigned size, char*(&table)[size]> void test1() { }
void tester1() { test1<10,table1>(); }

static char* table2[10];
template<unsigned size, char*(&table)[size]> void test2() { }
void tester2() { test2<10,table2>(); }

const char* table3[10];
template<unsigned size, const char*(&table)[size]> void test3() { }
void tester3() { test3<10,table3>(); }

const char* const table4[10] = {};
template<unsigned size, const char*const (&table)[size]> void test4() { }
void tester4() { test4<10,table4>(); }

const char* volatile table5[10] = {};
template<unsigned size, const char* volatile (&table)[size]> void test5() { }
void tester5() { test5<10,table5>(); }

const char* const table6[10] = {};
template<unsigned size, const char*const (&table)[size]> void test6() { }
void tester6() { test6<10,table6>(); }
