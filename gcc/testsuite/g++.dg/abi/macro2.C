// { dg-options "-fabi-version=2" }

#if __GXX_ABI_VERSION != 1002
#error "Incorrect value of __GXX_ABI_VERSION"
#endif
