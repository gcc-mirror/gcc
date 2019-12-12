/* { dg-do compile } */
/* { dg-skip-if "No section attribute" { { hppa*-*-hpux* } && { ! lp64 } } } */
/* { dg-options "-fpic -Os -g" } */
/* { dg-require-effective-target fpic } */

struct fit_loadable_tbl {
	int type;
	void (*handler)(int data, int size);
};

#define ll_entry_start(_type, _list)					\
({									\
	static char start[0] __attribute__((aligned(4)))		\
		__attribute__((unused, section(".u_boot_list_2_"#_list"_1")));	\
	(_type *)&start;						\
})

#define ll_entry_end(_type, _list)					\
({									\
	static char end[0] __attribute__((aligned(4)))			\
		__attribute__((unused, section(".u_boot_list_2_"#_list"_3")));	\
	(_type *)&end;							\
})

#define ll_entry_count(_type, _list)					\
	({								\
		_type *start = ll_entry_start(_type, _list);		\
		_type *end = ll_entry_end(_type, _list);		\
		unsigned int _ll_result = end - start;			\
		_ll_result;						\
	})

void test(int img_type, int img_data, int img_len)
{
	int i;
	const unsigned int count =
		ll_entry_count(struct fit_loadable_tbl, fit_loadable);
	struct fit_loadable_tbl *fit_loadable_handler =
		ll_entry_start(struct fit_loadable_tbl, fit_loadable);

	for (i = 0; i < count; i++, fit_loadable_handler++)
		if (fit_loadable_handler->type == img_type)
			fit_loadable_handler->handler(img_data, img_len);
}
