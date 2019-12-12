
/* REQUIRED_ARGS:
   PERMUTE_ARGS:
 */

// https://issues.dlang.org/show_bug.cgi?id=16538

const(int) retConst1();
int retConst2();
auto retConst = [&retConst1, &retConst2];

const(int*) retConstPtr1();
const(int)* retConstPtr2();
auto retConstPtr = [&retConstPtr1, &retConstPtr2];

void constArray1(const(int)[1]);
void constArray2(const(int[1]));
auto constArray = [&constArray1, &constArray2];

const(int)[] retConstSlice1();
const(int[]) retConstSlice2();
auto retConstSlice = [&retConstSlice1, &retConstSlice2];

void constSlice1(const(int)[]);
void constSlice2(const(int[]));
auto constSlice = [&constSlice1, &constSlice2];

void ptrToConst1(const(int)*);
void ptrToConst2(const(int*));
auto ptrToConst = [&ptrToConst1, &ptrToConst2];
