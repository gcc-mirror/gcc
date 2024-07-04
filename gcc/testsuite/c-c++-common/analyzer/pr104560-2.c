struct ibv_device {
  /* [...snip...] */
  int placeholder;
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
  return (struct mlx5_device *)ibdev;
}
  
static void mlx5_uninit_device(struct verbs_device *verbs_device)
{
  struct mlx5_device *dev = to_mdev(&verbs_device->device);
  __builtin_free(dev); /* { dg-bogus "not on the heap" } */
}
