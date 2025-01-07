// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh
import std.stdio;

/// Example
/// ---
/// void main() {
/// 	foreach (i; 0..10) {
/// 		writeln("Hello, world!");
/// 	}
/// }
/// ---
void main() {

    writeln("Hello, World!");

}
