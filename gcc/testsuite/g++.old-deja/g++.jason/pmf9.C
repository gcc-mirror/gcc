// { dg-do assemble  }
// PRMS id: g++/13340

class rectangle {
    
public:
  rectangle();
  int overlaps() const;

};

class region 
{
  friend class region_impl;

public:
  region();
  typedef int (region::* region_func)() const;      

};

class region_impl  {
  friend class region;

private:
  rectangle content, mbb;
  region_impl *link_p;
  region_impl(const rectangle &content);

public:
  int iterate(region *region_p, region::region_func what,
	      const rectangle &clip_rect) const;
  int iterate(region *region_p, region::region_func what,
	      const region_impl &clip_rgn) const;
};


int
region_impl::iterate (region *region_p, region::region_func what, 
		      const rectangle &clip_rect) const
{
  for (const region_impl *p = this; p != 0 && p->mbb.overlaps();
       p = p->link_p)
    if (p->content.overlaps())
      if (!(region_p->*what)()) return 0;
  return 1;
}

int
region_impl::iterate (region *region_p, region::region_func what,
		      const region_impl &clip_rgn) const
{
  for (const region_impl *p = this; p != 0 && p->mbb.overlaps();
       p = p->link_p)
    if (!clip_rgn.iterate(region_p, what, p->content)) return 0;
  return 1;
}
