// Check to make sure we align virtual base classes properly

class eel_base {
public:
};

class markable_eel_base : public eel_base {
private:
  int mark;
};

class eel_edge : public markable_eel_base {
public:
private:
  int foo;
};

class edge : public virtual eel_edge {
public:
  edge() {
    _weight = 0.0;
  }
private:
  double _weight;
};
class eel_branch_edge : public virtual edge {
};
class branch_edge : public eel_branch_edge {
};

class eel_interproc_branch_edge : public branch_edge {
};

class interproc_edge : public virtual edge {
};

class eel_jump_edge : public virtual edge {
protected:
};

class jump_edge : public eel_jump_edge {
public:
};

class eel_interproc_jump_edge : public jump_edge {
protected:
};

class interproc_jump_edge : public eel_interproc_jump_edge,
                            public interproc_edge {
public:
};

int main () {
  void *vp = new interproc_jump_edge();
}
