/* Extracted from PR middle-end/11771.  */
/* The following testcase used to ICE without -ffast-math from unbounded
   recursion in fold.  This was due to the logic in negate_expr_p not
   matching that in negate_expr.  */

double f(double x) {
    return -(1 - x) + (x ? -(1 - x) : 0);
}

