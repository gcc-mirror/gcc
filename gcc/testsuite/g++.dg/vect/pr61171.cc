// { dg-do compile }
// { dg-require-effective-target vect_float }
// { dg-additional-options "-ffast-math" }

float px[1024];
float xx, vv;
unsigned int N=1024;

void ok() {
    for (unsigned j=0U; j<N; ++j) {
	float ax = px[j]-xx;
	vv-=ax;
    }
}

void noOk() {
    for (unsigned j=0U; j<N; ++j) {
	float ax = xx-px[j];
	vv+=ax;
    }
}

// { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } }
