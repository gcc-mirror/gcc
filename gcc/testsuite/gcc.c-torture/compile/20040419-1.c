/* This used to ICE because PHI-OPT would produce non-gimple code. */

int f(double d0, double d1) { return d0 > 0 == d1 > 0; }
