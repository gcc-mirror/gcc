// Build don't link:

class Co_obj {
public:
   Co_obj() {}
 };
class Linear_list_node : virtual public Co_obj { };
class M1 : public Linear_list_node { };
class M2 : public Linear_list_node { };
class New_node : public M1, public M2 { };
