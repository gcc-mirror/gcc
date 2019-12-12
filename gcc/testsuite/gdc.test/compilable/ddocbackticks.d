// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh backticks

/++
        Closely related to std.datetime is <a href="core_time.html">`core.time`</a>,
    and some of the time types used in std.datetime come from there - such as
    $(CXREF time, Duration), $(CXREF time, TickDuration), and
    $(CXREF time, FracSec).
    core.time is publically imported into std.datetime, it isn't necessary
    to import it separately.
+/
module ddocbackticks;

/// This should produce `inline code`.
void test() {}

/// But `this should NOT be inline'
///
/// However, restarting on a new line should be `inline again`.
void test2() {}

/// This `int foo;` should show highlight on foo, but not int.
void foo() {}
