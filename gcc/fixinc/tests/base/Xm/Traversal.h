

#if defined( X11_NEW_CHECK )
struct wedge {
#ifdef __cplusplus
	Widget	old, c_new;
#else
   Widget	old, new; /* fix the new */
#endif
};
extern Wedged( Widget c_new, Widget old );
#endif  /* X11_NEW_CHECK */
