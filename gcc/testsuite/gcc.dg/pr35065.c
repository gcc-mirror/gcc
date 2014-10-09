/* PR debug/35065 */
/* { dg-do compile } */
/* { dg-options "-O2 -g -funroll-loops" } */
typedef int vlc_bool_t;
typedef __SIZE_TYPE__ size_t;
typedef struct vlc_object_t vlc_object_t;
typedef long long int64_t;
#if(__SIZEOF_INT__ >= 4)
typedef unsigned int uint32_t;
#else
typedef unsigned long uint32_t;
#endif
typedef unsigned char uint8_t;
typedef int64_t mtime_t;
typedef uint32_t vlc_fourcc_t;
typedef struct module_t module_t;
typedef struct es_format_t es_format_t;
typedef struct decoder_t decoder_t;
typedef struct decoder_sys_t decoder_sys_t;
typedef struct block_t block_t;
extern void* malloc (size_t);
enum vlc_module_properties {
  VLC_MODULE_CB_OPEN, VLC_MODULE_CB_CLOSE, VLC_MODULE_NAME, };
struct es_format_t {
  vlc_fourcc_t i_codec;
  int i_extra;
  void *p_extra;
};
struct block_t {
  block_t *p_next;
  uint32_t i_flags;
  mtime_t i_pts;
  mtime_t i_dts;
  size_t i_buffer;
  uint8_t *p_buffer;
};
block_t* block_New(void*, size_t);
block_t *nal_get_annexeb(decoder_t *, uint8_t *, int);
block_t *block_ChainGather (block_t *);
static inline block_t *block_Duplicate( block_t *p_block ) {
  block_t *p_dup = block_New( ((void *)0), p_block->i_buffer );
  p_dup->i_dts = p_block->i_dts;
  p_dup->i_pts = p_block->i_pts;
}
static inline void block_ChainAppend( block_t **pp_list, block_t *p_block ) {
  if( *pp_list == ((void *)0) ) {
  }
  else {
    block_t *p = *pp_list;
    while( p->p_next ) p = p->p_next;
    p->p_next = p_block;
  }
}
struct decoder_t {
  decoder_sys_t * p_sys;
  es_format_t fmt_in;
};
typedef struct bs_s {
  uint8_t *p;
  uint8_t *p_end;
  int i_left;
} bs_t;
static inline uint32_t bs_read( bs_t *s, int i_count ) {
  static uint32_t i_mask[33] = {
    0x00, 0x1fffffff,0x3fffffff,0x7fffffff,0xffffffff};
  int i_shr;
  uint32_t i_result = 0;
  while( i_count > 0 ) {
    if( s->p >= s->p_end ) {
      break;
    }
    if( ( i_shr = s->i_left - i_count ) >= 0 ) {
      i_result |= ( *s->p >> i_shr )&i_mask[i_count];
      s->i_left -= i_count;
      {
	s->i_left = 8;
      }
      return( i_result );
    }
    {
      i_result |= (*s->p&i_mask[s->i_left]) << -i_shr;
      i_count -= s->i_left;
      s->p++;
    }
  }
}
static inline uint32_t bs_read1( bs_t *s ) {
  if( s->p < s->p_end ) {
    unsigned int i_result;
    s->i_left--;
    i_result = ( *s->p >> s->i_left )&0x01;
    if( s->i_left == 0 ) {
      s->p++;
    }
    return i_result;
  }
  return 0;
}
int Open ( vlc_object_t * );
int vlc_module_set ( module_t *, enum vlc_module_properties, void *);
void Close( vlc_object_t * );
__attribute__((visibility("default"))) int vlc_entry__0_9_0f ( module_t *p_module ) {
  {
    module_t *p_submodule = p_module;
    if (vlc_module_set (p_submodule, VLC_MODULE_CB_OPEN, (void *)(Open)) || vlc_module_set (p_submodule, VLC_MODULE_CB_CLOSE, (void *)(Close))) goto error;
  }
 error:
  return -666;
}
typedef struct {
  int i_nal_type;
  int i_nal_ref_idc;
  int i_frame_type;
  int i_frame_num;
  int i_bottom_field_flag;
  int i_idr_pic_id;
  int i_delta_pic_order_cnt0;
} slice_t;
struct decoder_sys_t {
  vlc_bool_t b_slice;
  block_t *p_frame;
  vlc_bool_t b_sps;
  vlc_bool_t b_pps;
  vlc_bool_t b_header;
  block_t *p_sps;
  block_t *p_pps;
  int i_pic_order_cnt_type;
  slice_t slice;
};
enum { NAL_SLICE = 1, NAL_SLICE_IDR = 5, NAL_SPS = 7, NAL_AU_DELIMITER= 9 };
static block_t *ParseNALBlock( decoder_t *, block_t * );
int U16_AT ( uint8_t * );
int Open( vlc_object_t *p_this ) {
  decoder_t *p_dec = (decoder_t*)p_this;
  decoder_sys_t *p_sys;
  if( p_dec->fmt_in.i_codec != ( ((uint32_t)'h') | ( ((uint32_t)'2') << 8 ) | ( ((uint32_t)'6') << 16 ) | ( ((uint32_t)'4') << 24 ) ) && ( p_dec->fmt_in.i_codec != ( ((uint32_t)'a') | ( ((uint32_t)'v') << 8 ) | ( ((uint32_t)'c') << 16 ) | ( ((uint32_t)'1') << 24 ) ) || p_dec->fmt_in.i_extra < 7 ) ) {
    return -666;
  }
  if( ( p_dec->p_sys = p_sys = malloc( sizeof(decoder_sys_t) ) ) == ((void *)0) ) {
    uint8_t *p = &((uint8_t*)p_dec->fmt_in.p_extra)[4];
    int i_sps, i_pps;
    int i;
    i_sps = (*p++)&0x1f;
    for( i = 0;
	 i < i_sps;
	 i++ ) {
      int i_length = U16_AT( p );
      block_t *p_sps = nal_get_annexeb( p_dec, p + 2, i_length );
      ParseNALBlock( p_dec, p_sps );
    }
  }
}
static inline int bs_read_ue( bs_t *s ) {
  int i = 0;
  while( bs_read1( s ) == 0 && s->p < s->p_end && i < 32 ) {
    i++;
  }
  return( ( 1 << i) - 1 + bs_read( s, i ) );
}
static inline int bs_read_se( bs_t *s ) {
  int val = bs_read_ue( s );
}
block_t *ParseNALBlock( decoder_t *p_dec, block_t *p_frag )
{
  decoder_sys_t *p_sys = p_dec->p_sys;
  block_t *p_pic = ((void *)0);
  const int i_nal_type = p_frag->p_buffer[4]&0x1f;
  if( ( !p_sys->b_sps || !p_sys->b_pps ) && i_nal_type >= NAL_SLICE && i_nal_type <= NAL_SLICE_IDR ) {
  }
  else if( i_nal_type >= NAL_SLICE && i_nal_type <= NAL_SLICE_IDR ) {
    int i_dec = 0, i_first_mb, i_slice_type;
    slice_t slice;
    bs_t s;
    i_first_mb = bs_read_ue( &s );
    switch( (i_slice_type = bs_read_ue( &s )) ) {
    }
    if( p_sys->i_pic_order_cnt_type == 0 ) {
      slice.i_delta_pic_order_cnt0 = bs_read_se( &s );
    }
    if( slice.i_frame_num != p_sys->slice.i_frame_num
	|| slice.i_nal_ref_idc != p_sys->slice.i_nal_ref_idc )
      if( (slice.i_bottom_field_flag != -1)
	  && (slice.i_bottom_field_flag != p_sys->slice.i_bottom_field_flag) )
	if( p_sys->i_pic_order_cnt_type == 0 && ( slice.i_nal_type != p_sys->slice.i_nal_type || slice.i_idr_pic_id != p_sys->slice.i_idr_pic_id ) )
	  do {
	    if( !p_sys->b_header && p_sys->slice.i_frame_type != 0x0002)
	      break;
	    if( p_sys->slice.i_frame_type == 0x0002 && p_sys->p_sps && p_sys->p_pps ) {
	      block_t *p_sps = block_Duplicate( p_sys->p_sps );
	      block_t *p_pps = block_Duplicate( p_sys->p_pps );
	      p_sps->i_pts = p_sys->p_frame->i_pts;
	      block_ChainAppend( &p_sps, p_pps );
	      block_ChainAppend( &p_sps, p_sys->p_frame );
	      p_sys->b_header = 1;
	      p_pic = block_ChainGather( p_sps );
	    }
	  } while(0);
  }
  else if( i_nal_type == NAL_SPS ) {
    bs_t s;
    if( p_sys->i_pic_order_cnt_type == 0 ) {
    }
    else if( p_sys->i_pic_order_cnt_type == 1 ) {
      int i_cycle;
      i_cycle = bs_read_ue( &s );
      while( i_cycle > 0 ) {
	bs_read_se(&s );
      }
    }
    bs_read_ue( &s );
    if( p_sys->b_slice )
      do {
	if( !p_sys->b_header && p_sys->slice.i_frame_type != 0x0002)
	  break;
	if( p_sys->slice.i_frame_type == 0x0002
	    && p_sys->p_sps && p_sys->p_pps )
	  {
	    block_t *p_sps = block_Duplicate( p_sys->p_sps );
	    block_t *p_pps = block_Duplicate( p_sys->p_pps );
	    p_sps->i_dts = p_sys->p_frame->i_dts;
	    p_sps->i_pts = p_sys->p_frame->i_pts;
	    block_ChainAppend( &p_sps, p_pps );
	    block_ChainAppend( &p_sps, p_sys->p_frame );
	    p_pic = block_ChainGather( p_sps );
	  }
	p_pic->i_flags |= p_sys->slice.i_frame_type;
      } while(0);
  }
  block_ChainAppend( &p_sys->p_frame, p_frag );
}
