/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

void pthread_mutex_unlock(int *);
int __gthread_mutex_unlock___mutex, unlock___trans_tmp_1;
struct Object {
  void _change_notify() {}
  bool _is_queued_for_deletion;
};
struct ClassDB {
  template <class N, class M> static int bind_method(N, M);
};
struct CanvasItemMaterial : Object {
  bool particles_animation;
  void set_particles_animation(bool);
};
void CanvasItemMaterial::set_particles_animation(bool p_particles_anim) {
  particles_animation = p_particles_anim;
  if (unlock___trans_tmp_1)
    pthread_mutex_unlock(&__gthread_mutex_unlock___mutex);
  _change_notify();
}
void CanvasItemMaterial_bind_methods() {
  ClassDB::bind_method("", &CanvasItemMaterial::set_particles_animation);
}
