// Build don't link:
// prms-id: 4671

class ccUnwind {
public:
  virtual void _c_getInfo() const;
  virtual ~ccUnwind ();
};
class ccTransmittable {
public:
  virtual ~ccTransmittable();
};
class ccCommand : public ccUnwind, public ccTransmittable {
};
class foo : public ccCommand {
};
