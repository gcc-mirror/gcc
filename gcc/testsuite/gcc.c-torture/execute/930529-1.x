# The problem on Alpha at -O3 is that when dd is inlined, we have
# division by a constant, which gets converted to multiplication
# by a large constant, which gets turned into an induction variable.
# The problem is that the multiplication was unsigned SImode, and the
# induction variable is DImode, and we lose the truncation that
# should have happened.
#
# On tree-ssa branch, the loop problem is still extant, but the
# gimple-level optimization makes it easy for the tree-rtl expanders
# to see that the comparisons are always true, and so the loop code
# is never exercized.

# set torture_eval_before_execute {
# 
#     set compiler_conditional_xfail_data {
#         "division by a constant conflicts with strength reduction" \
#         "alpha*-*-*" \
#         { "-O3" } \
#         { "" }
#     }
# }

return 0
