// { dg-options "-fabi-version=1" }

#if __GXX_ABI_VERSION != 102
#error "Incorrect value of __GXX_ABI_VERSION"
#endif
