typedef unsigned int	__kernel_dev_t;
typedef __kernel_dev_t		dev_t;
struct ustat {
};
typedef unsigned int kdev_t;
static inline kdev_t to_kdev_t(int dev)
{
	int major, minor;
	major = (dev >> 8);
	minor = (dev & 0xff);
	return ((( major ) << 22 ) | (  minor )) ;
}
struct super_block {
};
struct super_block * get_super (kdev_t dev);
int sys_ustat(dev_t dev, struct ustat * ubuf)
{
        struct super_block *s;
        s = get_super(to_kdev_t(dev));
}
