// { dg-options "-fabi-version=0" }

#if __GXX_ABI_VERSION != 999999
#error "Incorrect value of __GXX_ABI_VERSION"
#endif
