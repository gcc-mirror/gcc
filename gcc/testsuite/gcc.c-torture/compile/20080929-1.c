struct option {
  const char *name;
  int has_arg;
  int *flag;
  int val;
};
enum {
  UBI_DYNAMIC_VOLUME = 3,  UBI_STATIC_VOLUME = 4, };
typedef void * libubi_t;
struct ubi_dev_info {
  int leb_size;
};
struct args {
  int vol_id;
  int vol_type;
  long long bytes;
  int lebs;
  int alignment;
  const char *node;
  int maxavs;
  int devn;
};
static struct args args = {
  .vol_type = UBI_DYNAMIC_VOLUME,  .bytes = -1,  .lebs = -1,  .alignment = 1,  .vol_id = (-1),  .devn = -1, };
extern libubi_t libubi_open (int);
extern int ubi_get_dev_info (libubi_t, const char *, struct ubi_dev_info *);
int main(int argc, char * const argv[]) {
  int err;
  libubi_t libubi;
  struct ubi_dev_info dev_info;
  libubi = libubi_open(1);
  if (!libubi)
    return 0;
  err = ubi_get_dev_info(libubi, args.node, &dev_info);
  if (args.maxavs) {
    args.bytes = dev_info.leb_size;
  }
  return 0;
}
