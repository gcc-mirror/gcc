/* Reduced test case from PR53703.  Used to ICE.  */

/* { dg-do compile } */
/* { dg-options "-w" } */

typedef long unsigned int size_t;
typedef unsigned short int sa_family_t;
struct sockaddr   {};
typedef unsigned char __u8;
typedef unsigned short __u16;
typedef unsigned int __u32;
struct nlmsghdr {
  __u32 nlmsg_len;
  __u16 nlmsg_type;
};
struct ifaddrmsg {
  __u8 ifa_family;
};
enum {
  IFA_ADDRESS,
  IFA_LOCAL,
};
enum {
  RTM_NEWLINK = 16,
  RTM_NEWADDR = 20,
};
struct rtattr {
  unsigned short rta_len;
  unsigned short rta_type;
};
struct ifaddrs {
  struct ifaddrs *ifa_next;
  unsigned short ifa_flags;
};
typedef unsigned short int uint16_t;
typedef unsigned int uint32_t;
struct nlmsg_list {
  struct nlmsg_list *nlm_next;
  int size;
};
struct rtmaddr_ifamap {
  void *address;
  void *local;
  int address_len;
  int local_len;
};
int usagi_getifaddrs (struct ifaddrs **ifap)
{
  struct nlmsg_list *nlmsg_list, *nlmsg_end, *nlm;
  size_t dlen, xlen, nlen;
  int build;
  for (build = 0; build <= 1; build++)
    {
      struct ifaddrs *ifl = ((void *)0), *ifa = ((void *)0);
      struct nlmsghdr *nlh, *nlh0;
      uint16_t *ifflist = ((void *)0);
      struct rtmaddr_ifamap ifamap;
      for (nlm = nlmsg_list; nlm; nlm = nlm->nlm_next)
	{
	  int nlmlen = nlm->size;
	  for (nlh = nlh0;
	       ((nlmlen) >= (int)sizeof(struct nlmsghdr)
		&& (nlh)->nlmsg_len >= sizeof(struct nlmsghdr)
		&& (nlh)->nlmsg_len <= (nlmlen));
	       nlh = ((nlmlen) -= ( (((nlh)->nlmsg_len)+4U -1) & ~(4U -1) ),
		      (struct nlmsghdr*)(((char*)(nlh))
					 + ( (((nlh)->nlmsg_len)+4U -1)
					     & ~(4U -1) ))))
	    {
	      struct ifinfomsg *ifim = ((void *)0);
	      struct ifaddrmsg *ifam = ((void *)0);
	      struct rtattr *rta;
	      sa_family_t nlm_family = 0;
	      uint32_t nlm_scope = 0, nlm_index = 0;
	      memset (&ifamap, 0, sizeof (ifamap));
	      switch (nlh->nlmsg_type)
		{
		case RTM_NEWLINK:
		  ifim = (struct ifinfomsg *)
		    ((void*)(((char*)nlh)
			     + ((0)+( ((((int)
					 ( ((sizeof(struct nlmsghdr))+4U -1)
					   & ~(4U -1) )))+4U -1)
				      & ~(4U -1) ))));
		case RTM_NEWADDR:
		  ifam = (struct ifaddrmsg *)
		    ((void*)(((char*)nlh)
			     + ((0)+( ((((int)
					 ( ((sizeof(struct nlmsghdr))+4U -1)
					   & ~(4U -1) )))+4U -1)
				      & ~(4U -1) ))));
		  nlm_family = ifam->ifa_family;
		  if (build)
		    ifa->ifa_flags = ifflist[nlm_index];
		  break;
		default:
		  continue;
		}
	      if (!build)
		{
		  void *rtadata = ((void*)(((char*)(rta))
					   + (( ((sizeof(struct rtattr))+4 -1)
						& ~(4 -1) ) + (0))));
		  size_t rtapayload = ((int)((rta)->rta_len)
				       - (( ((sizeof(struct rtattr))+4 -1)
					    & ~(4 -1) ) + (0)));
		  switch (nlh->nlmsg_type)
		    {
		    case RTM_NEWLINK:
		      break;
		    case RTM_NEWADDR:
		      if (nlm_family == 17)
			break;
		      switch (rta->rta_type)
			{
			case IFA_ADDRESS:
			  ifamap.address = rtadata;
			  ifamap.address_len = rtapayload;
			case IFA_LOCAL:
			  ifamap.local = rtadata;
			}
		    }
		}
	      if (nlh->nlmsg_type == RTM_NEWADDR && nlm_family != 17)
		{
		  if (!ifamap.local)
		    {
		      ifamap.local = ifamap.address;
		      ifamap.local_len = ifamap.address_len;
		    }
		  if (!ifamap.address)
		    {
		      ifamap.address = ifamap.local;
		    }
		  if (ifamap.address_len != ifamap.local_len
		      || (ifamap.address != ((void *)0)
			  && memcmp (ifamap.address, ifamap.local,
				     ifamap.address_len)))
		    {
		      if (!build)
			dlen += (((ifa_sa_len (nlm_family,
					       ifamap.address_len))+4U -1)
				 & ~(4U -1) );
		    }
		}
	    }
	}
    }
}
