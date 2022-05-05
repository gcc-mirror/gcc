/* Reduced/adapted from false positive from -Wanalyzer-free-of-non-heap
   seen on rdma-core.  */

#include <stddef.h>

#define check_types_match(expr1, expr2)			\
	((typeof(expr1) *)0 != (typeof(expr2) *)0)

#define container_of(member_ptr, containing_type, member)		\
	 ((containing_type *)						\
	  ((char *)(member_ptr)						\
	   - container_off(containing_type, member))			\
	  + check_types_match(*(member_ptr), ((containing_type *)0)->member))

#define container_off(containing_type, member)	\
	offsetof(containing_type, member)

struct ibv_device {
	/* [...snip...] */
};

struct verbs_device {
	struct ibv_device device; /* Must be first */
	/* [...snip...] */
	int placeholder;
};

struct mlx5_device {
	struct verbs_device verbs_dev;
	int placeholder;
};

static inline struct mlx5_device *to_mdev(struct ibv_device *ibdev)
{
	return container_of(ibdev, struct mlx5_device, verbs_dev.device);
}
  
static void mlx5_uninit_device(struct verbs_device *verbs_device)
{
        struct mlx5_device *dev = to_mdev(&verbs_device->device);

        __builtin_free(dev); /* { dg-bogus "not on the heap" } */
}
