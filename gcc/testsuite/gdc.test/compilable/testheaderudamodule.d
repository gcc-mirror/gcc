// REQUIRED_ARGS: -o- -H -Hf${RESULTS_DIR}/compilable/testheaderudamodule.di
// PERMUTE_ARGS:
// POST_SCRIPT: compilable/extra-files/header-postscript.sh testheaderudamodule

@(1, UDA(2))
module testheaderudamodule;

struct UDA
{
    int a;
}

void main() {}
