/* Reduced from SoftEtherVPN's src/Cedar/WebUI.c.   */

#define NULL ((void *)0)
typedef int (COMPARE)(void *p1, void *p2);
typedef unsigned int UINT;
typedef unsigned long int UINT64;
typedef struct LIST LIST;
typedef struct STRMAP_ENTRY STRMAP_ENTRY;
typedef struct WEBUI
{
	/* [...snip...] */
	LIST *Contexts;
} WEBUI;

typedef struct WU_CONTEXT
{
	/* [...snip...] */
	UINT64 ExpireDate;
} WU_CONTEXT;

struct LIST
{
	/* [...snip...] */
	UINT num_item, num_reserved;
	void **p;
	/* [...snip...] */
};

#define	LIST_DATA(o, i)		(((o) != NULL) ? ((o)->p[(i)]) : NULL)
#define	LIST_NUM(o)			(((o) != NULL) ? (o)->num_item : 0)


struct STRMAP_ENTRY
{
	char *Name;
	void *Value;
};

void Free(void *addr);
void Add(LIST *o, void *p);
_Bool Delete(LIST *o, void *p);
void LockList(LIST *o);
void UnlockList(LIST *o);
void ReleaseList(LIST *o);
LIST *NewList(COMPARE *cmp);
UINT64 Tick64();
void WuFreeContext(WU_CONTEXT *context);

void WuExpireSessionKey(WEBUI *wu)
{
	LIST *Expired = NewList(NULL);
	UINT i;

	LockList(wu->Contexts);

	for(i=0; i<LIST_NUM(wu->Contexts); i++)
	{
		STRMAP_ENTRY *entry = (STRMAP_ENTRY*)LIST_DATA(wu->Contexts, i);
		WU_CONTEXT *context = (WU_CONTEXT*)entry->Value; /* { dg-bogus "dereference of NULL 'entry'" "PR analyzer/108400" { xfail *-*-* } } */
		if(context->ExpireDate < Tick64())
		{
			Add(Expired, entry);
		}
	}

	for(i=0; i<LIST_NUM(Expired); i++)
	{
		STRMAP_ENTRY *entry = LIST_DATA(Expired, i);
		Delete(wu->Contexts, entry);
		Free(entry->Name);
		WuFreeContext(entry->Value);
		Free(entry);
	}
	ReleaseList(Expired);

	UnlockList(wu->Contexts);
}
