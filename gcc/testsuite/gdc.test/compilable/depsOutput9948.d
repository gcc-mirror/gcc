// PERMUTE_ARGS:
// REQUIRED_ARGS: -deps=${RESULTS_DIR}/compilable/depsOutput9948.deps
// POST_SCRIPT: compilable/extra-files/depsOutput.sh 
// EXTRA_SOURCES: extra-files/depsOutput9948a.d

module depsOutput9948;
import depsOutput9948a;

void main()
{
   templateFunc!("import std.string;")();
}
