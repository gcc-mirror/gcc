// REQUIRED_ARGS: -o- -H -Hf${RESULTS_DIR}/compilable/header12567b.di
// PERMUTE_ARGS:
// POST_SCRIPT: compilable/extra-files/header-postscript.sh header12567b

deprecated("message") module header12567b;

void main() {}
