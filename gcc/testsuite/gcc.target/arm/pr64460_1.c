/* { dg-do compile } */
/* { dg-options "-O2 -mtune=xscale" } */

typedef unsigned int size_t;
typedef short unsigned int __uint16_t;
typedef long unsigned int __uint32_t;
typedef unsigned int __uintptr_t;
typedef __uint16_t uint16_t ;
typedef __uint32_t uint32_t ;
typedef __uintptr_t uintptr_t;
typedef uint32_t Objects_Id;
typedef uint16_t Objects_Maximum;
typedef struct { } Objects_Control;

static __inline__ void *_Addresses_Align_up (void *address, size_t alignment)
{
	uintptr_t mask = alignment - (uintptr_t)1;
	return (void*)(((uintptr_t)address + mask) & ~mask);
}

typedef struct {
	Objects_Id minimum_id;
	Objects_Maximum maximum;
	_Bool
		auto_extend;
	Objects_Maximum allocation_size;
	void **object_blocks;
} Objects_Information;

extern uint32_t _Objects_Get_index (Objects_Id);
extern void** _Workspace_Allocate (size_t);

void _Objects_Extend_information (Objects_Information *information)
{
	uint32_t block_count;
	uint32_t minimum_index;
	uint32_t maximum;
	size_t block_size;
	_Bool
		do_extend =
		minimum_index = _Objects_Get_index( information->minimum_id );
	if ( information->object_blocks ==
			((void *)0)
	   )
		block_count = 0;
	else {
		block_count = information->maximum / information->allocation_size;
	}
	if ( do_extend ) {
		void **object_blocks;
		uintptr_t object_blocks_size;
		uintptr_t inactive_per_block_size;
		object_blocks_size = (uintptr_t)_Addresses_Align_up(
				(void*)(block_count * sizeof(void*)),
				8
				);
		inactive_per_block_size =
			(uintptr_t)_Addresses_Align_up(
					(void*)(block_count * sizeof(uint32_t)),
					8
					);
		block_size = object_blocks_size + inactive_per_block_size +
			((maximum + minimum_index) * sizeof(Objects_Control *));
		if ( information->auto_extend ) {
			object_blocks = _Workspace_Allocate( block_size );
		} else {
		}
	}
}
