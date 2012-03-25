// { dg-do compile }
template<class T> class intrusive_ptr {
public:
    ~intrusive_ptr() { intrusive_ptr_release( px ); }
    T * px;
};
template <typename T>     struct intrusive_base     {
    friend void intrusive_ptr_release(T* ptr) { delete ptr; }
};
struct section_info;
struct file_info : intrusive_base<file_info> {
    intrusive_ptr<file_info> parent;
    intrusive_ptr<section_info> switched_section;
};
struct section_info : intrusive_base<section_info> {
    intrusive_ptr<section_info> parent;
};
struct id_state {
    void * start_file(void);
};
void * id_state::start_file(void) {
    intrusive_ptr<file_info> parent;
}
struct id_generation_data : intrusive_base<id_generation_data> {
    void child_length() const {}
};
void generate_id(id_generation_data& generation_data)
{
  generation_data.child_length();
}
