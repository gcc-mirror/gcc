void __add_entropy_words(void);
void __wake_up(void);
void SHATransform(void);
static inline __attribute__((always_inline)) void add_entropy_words(void){}
void extract_entropy(void);
static inline __attribute__((always_inline)) void xfer_secondary_pool(void)
{
extract_entropy();
add_entropy_words();
}
void extract_entropy(void)
{
xfer_secondary_pool();
__wake_up();
}
void init_std_data(void)
{
add_entropy_words();
}
void rand_initialize(void)
{
init_std_data();
}
