#ifndef FIXINC_CXX_UNREADY_CHECK
#define FIXINC_CXX_UNREADY_CHECK 1

#ifdef __cplusplus
extern "C" {
#endif


#if defined( CXX_UNREADY_CHECK )
extern void* malloc( size_t );
#endif  /* CXX_UNREADY_CHECK */
#ifdef __cplusplus
}
#endif

#endif  /* FIXINC_CXX_UNREADY_CHECK */
