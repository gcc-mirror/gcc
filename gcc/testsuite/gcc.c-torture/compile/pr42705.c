typedef int GLint;
typedef unsigned char GLubyte;
typedef unsigned int uint32_t;
struct radeon_bo {
    void *ptr;
    uint32_t flags;
};
struct radeon_renderbuffer {
    struct radeon_bo *bo;
    unsigned int cpp;
    int has_surface;
};
static inline
GLint r600_1d_tile_helper(const struct radeon_renderbuffer * rrb,
			  GLint x, GLint y, GLint is_depth, GLint is_stencil)
{
  GLint element_bytes = rrb->cpp;
  GLint num_samples = 1;
  GLint tile_width = 8;
  GLint tile_height = 8;
  GLint tile_thickness = 1;
  GLint tile_bytes;
  GLint tiles_per_row;
  GLint slice_offset;
  GLint tile_row_index;
  GLint tile_column_index;
  GLint tile_offset;
  GLint pixel_number = 0;
  GLint element_offset;
  GLint offset = 0;
  tile_bytes = tile_width * tile_height * tile_thickness
      * element_bytes * num_samples;
  tile_column_index = x / tile_width;
  tile_offset = ((tile_row_index * tiles_per_row)
		 + tile_column_index) * tile_bytes;
  if (is_depth) {
  }
  else {
      GLint sample_offset;
      switch (element_bytes) {
	  case 1:       pixel_number |= ((x >> 0) & 1) << 0;
      }
      element_offset = sample_offset + (pixel_number * element_bytes);
  }
  offset = slice_offset + tile_offset + element_offset;
  return offset;
}
GLubyte *r600_ptr_color(const struct radeon_renderbuffer * rrb,
			GLint x, GLint y)
{
  GLubyte *ptr = rrb->bo->ptr;
  uint32_t mask = 1 | 2;
  GLint offset;
  if (rrb->has_surface || !(rrb->bo->flags & mask)) {
      offset = r600_1d_tile_helper(rrb, x, y, 0, 0);
  }
  return &ptr[offset];
}
