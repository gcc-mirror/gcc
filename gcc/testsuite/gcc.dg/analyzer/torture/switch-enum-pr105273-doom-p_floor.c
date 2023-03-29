/* Reduced from linuxdoom-1.10's p_floor.c (GPLv2).  */

#define FRACBITS		16
#define FRACUNIT		(1<<FRACBITS)
#define PU_LEVSPEC		51
#define FLOORSPEED		FRACUNIT

typedef int fixed_t;
typedef struct line_s line_t;

typedef struct
{
    fixed_t floorheight;
    /* [...snip...] */
} sector_t;

typedef enum
{
    build8,
    turbo16

} stair_e;

typedef struct
{
    /* [...snip...] */
    fixed_t floordestheight;
    fixed_t speed;
} floormove_t;

extern sector_t* sectors;

void* Z_Malloc (int size, int tag, void *ptr);

int
P_FindSectorFromLineTag
( line_t* line,
  int start );

int
EV_BuildStairs
( line_t*	line,
  stair_e	type )
{
    int			secnum;
    int			height;
    /* [...snip...] */
    int			rtn;
    
    sector_t*		sec;
    /* [...snip...] */

    floormove_t*	floor;
    
    fixed_t		stairsize;
    fixed_t		speed;

    secnum = -1;
    rtn = 0;
    while ((secnum = P_FindSectorFromLineTag(line,secnum)) >= 0)
    {
	sec = &sectors[secnum];

	/* [...snip...] */

	rtn = 1;
	floor = Z_Malloc (sizeof(*floor), PU_LEVSPEC, 0);
	
	/* [...snip...] */

	switch(type)
	{
	  case build8:
	    speed = FLOORSPEED/4;
	    stairsize = 8*FRACUNIT;
	    break;
	  case turbo16:
	    speed = FLOORSPEED*4;
	    stairsize = 16*FRACUNIT;
	    break;
	}
	floor->speed = speed; /* { dg-bogus "use of uninitialized value 'speed'" } */
	height = sec->floorheight + stairsize; /* { dg-bogus "use of uninitialized value 'stairsize'" } */
	floor->floordestheight = height;

	/* [...snip...] */
    }
    return rtn;
}
