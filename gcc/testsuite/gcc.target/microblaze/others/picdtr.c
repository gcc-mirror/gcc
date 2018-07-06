/* { dg-options "-fPIE -mpic-data-text-rel -save-temps" } */
/* { dg-do run } */

#define TEST_VAR(var,val) (var) = (val); if( (var) != (val)) return 0;

int foo(unsigned int i);
extern void abort(void);
extern void exit(int);

unsigned char data[8];
long bigData[7];
long var;
typedef struct {int a; short b; long c[1000][1000]; long long d[3][3]; char e; } myDef;
myDef def;
const char* myString;

/* { dg-final { scan-assembler "mfs\tr20,rpc" } } */
/* { dg-final { scan-assembler "addik\tr20,r20,8@TXTPCREL" } } */
/* { dg-final { scan-assembler ",r20,\[^\n]*var\[^\n]*@TXTREL" } } */
/* { dg-final { scan-assembler-not ",r0,\[^\n]*var" } } */
/* { dg-final { scan-assembler ",r20,\[^\n]*bigData\[^\n]*@TXTREL" } } */
/* { dg-final { scan-assembler-not ",r0,\[^\n]*bigData" } } */
/* { dg-final { scan-assembler ",r20,\[^\n]*def\[^\n]*@TXTREL" } } */
/* { dg-final { scan-assembler-not ",r0,\[^\n]*def" } } */
/* { dg-final { scan-assembler ",r20,\[^\n]*data\[^\n]*@TXTREL" } } */
/* { dg-final { scan-assembler-not ",r0,\[^\n]*data" } } */
/* { dg-final { scan-assembler ",r20,\[^\n]*L\[^\n]*@TXTREL" } } */
/* { dg-final { scan-assembler-not ",r0,\[^\n]*L" } } */



void foo2() {
	var++;
}

int foo (unsigned int i) {

	TEST_VAR(var,123)
	TEST_VAR(data[i],77)
	TEST_VAR(data[2],88)
	TEST_VAR(def.a,897)
	TEST_VAR(bigData[i],78)
	TEST_VAR(bigData[2],777)
	TEST_VAR(def.b,12333);
	TEST_VAR(def.c[i][i],5);
	TEST_VAR(def.c[0][1],7);
	TEST_VAR(def.d[1][2],123);
	TEST_VAR(def.e,7);
	TEST_VAR(bigData[i+1],bigData[i*2]);

	foo2();

	myString = "Hello";

	switch(i){

	case 1: var += 2; break;
	case 2: var += 3; break;
	case 3: var += 5; break;
	case 4: var += 7; break;
	case 5: var += 8; break;
	default: var = 0;

	}

	return 1;

}

int main() {

	int result = foo(3);
	if(result != 1 || var != 129) {
		abort();
	}

	exit(0);

}

/* { dg-options "-fPIE -mpic-data-is-text-relative -save-temps" } */
/* { dg-do run } */

#define TEST_VAR(var,val) (var) = (val); if( (var) != (val)) return 0;

int foo(unsigned int i);
extern void abort(void);
extern void exit(int);

unsigned char data[8];
long bigData[7];
long var;
typedef struct {int a; short b; long c[1000][1000]; long long d[3][3]; char e; } myDef;
myDef def;
const char* myString;

/* { dg-final { scan-assembler "mfs\tr20,rpc" } } */
/* { dg-final { scan-assembler "addik\tr20,r20,8@TXTPCREL" } } */
/* { dg-final { scan-assembler ",r20,\[^\n]*var\[^\n]*@TXTREL" } } */
/* { dg-final { scan-assembler-not ",r0,\[^\n]*var" } } */
/* { dg-final { scan-assembler ",r20,\[^\n]*bigData\[^\n]*@TXTREL" } } */
/* { dg-final { scan-assembler-not ",r0,\[^\n]*bigData" } } */
/* { dg-final { scan-assembler ",r20,\[^\n]*def\[^\n]*@TXTREL" } } */
/* { dg-final { scan-assembler-not ",r0,\[^\n]*def" } } */
/* { dg-final { scan-assembler ",r20,\[^\n]*data\[^\n]*@TXTREL" } } */
/* { dg-final { scan-assembler-not ",r0,\[^\n]*data" } } */
/* { dg-final { scan-assembler ",r20,\[^\n]*L\[^\n]*@TXTREL" } } */
/* { dg-final { scan-assembler-not ",r0,\[^\n]*L" } } */



void foo2() {
	var++;
}

int foo (unsigned int i) {

	TEST_VAR(var,123)
	TEST_VAR(data[i],77)
	TEST_VAR(data[2],88)
	TEST_VAR(def.a,897)
	TEST_VAR(bigData[i],78)
	TEST_VAR(bigData[2],777)
	TEST_VAR(def.b,12333);
	TEST_VAR(def.c[i][i],5);
	TEST_VAR(def.c[0][1],7);
	TEST_VAR(def.d[1][2],123);
	TEST_VAR(def.e,7);
	TEST_VAR(bigData[i+1],bigData[i*2]);

	foo2();

	myString = "Hello";

	switch(i){

	case 1: var += 2; break;
	case 2: var += 3; break;
	case 3: var += 5; break;
	case 4: var += 7; break;
	case 5: var += 8; break;
	default: var = 0;

	}

	return 1;

}

int main() {

	int result = foo(3);
	if(result != 1 || var != 129) {
		abort();
	}

	exit(0);

}

