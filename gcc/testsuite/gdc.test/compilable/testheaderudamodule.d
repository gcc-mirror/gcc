/*
REQUIRED_ARGS: -o- -H -Hf${RESULTS_DIR}/compilable/testheaderudamodule.di
PERMUTE_ARGS:
OUTPUT_FILES: ${RESULTS_DIR}/compilable/testheaderudamodule.di

TEST_OUTPUT:
---
=== ${RESULTS_DIR}/compilable/testheaderudamodule.di
// D import file generated from 'compilable/testheaderudamodule.d'
@(1, UDA(2))
module testheaderudamodule;
struct UDA
{
	int a;
}
void main();
void foo(@(1) int bar, @UDA(2) string bebe);
---
*/

@(1, UDA(2))
module testheaderudamodule;

struct UDA
{
    int a;
}

void main() {}

void foo(@(1) int bar, @UDA(2) string bebe) {}
