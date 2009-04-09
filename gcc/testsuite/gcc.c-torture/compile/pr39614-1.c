typedef struct page {
 unsigned long flags;
} mem_map_t;
static inline void set_page_zone(struct page *page, unsigned long zone_num)
{
 page->flags &= ~(~0UL << (64 - 8));
}
