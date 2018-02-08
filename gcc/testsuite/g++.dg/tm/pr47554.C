// { dg-do compile }
// { dg-options "-fgnu-tm" }

class list
{
	public:     list()
	{
	}
	list(const list&)
	{
	}
	const list&       _M_get_Node_allocator() const
	{
	  static list l;
	  return l;
	}
	list       _M_get_Tp_allocator() const
	{
		return list(_M_get_Node_allocator());
	}
};
static list buildProjects;
static void build()
{
	__transaction_relaxed
	{
		buildProjects._M_get_Tp_allocator();
	}
}
