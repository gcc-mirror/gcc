// Build don't link: 
/* 
The 971114 "gcc/cp/parse.y" doesn't properly identify non-aggregate
types used as base classes.

First, the rule:

        base_class: base_class_access_list see_typename base_class.1

uses "IS_AGGR_TYPE" instead of "is_aggr_type" to check "base_class.1",
so no error is reported for code like:

*/
        typedef int an_int;
        class bar : public an_int {};
