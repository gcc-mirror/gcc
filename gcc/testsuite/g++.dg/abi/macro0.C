// This testcase will need to be kept in sync with c_common_post_options.
// { dg-options "-fabi-version=0" }

#if __GXX_ABI_VERSION != 1011
#error "Incorrect value of __GXX_ABI_VERSION"
#endif
