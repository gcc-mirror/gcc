// Build don't link:
// prms-id: 3764

class Menu;
class MenuItem;

class  MenuAction {
public:
  virtual void execute (Menu& menu, MenuItem& menuItem)  = 0;
protected:
  MenuAction () {}
}; 

class Test {
  class MenuCBA : public MenuAction {
  public:
    typedef void (Test::* MenuCBA_Member) (Menu& menu, MenuItem& menuItem) ;
    MenuCBA(Test& instance, MenuCBA_Member member)
      : _instance(&instance), _member(member) { }
    void execute(Menu& menu, MenuItem& menuItem);
  private:
    MenuCBA_Member _member;
    Test *_instance;
  };  
};
