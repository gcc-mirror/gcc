struct keyring_list {
 struct key *keys[0];
};
void key_put(struct key *);
void keyring_destroy(struct keyring_list *keyring, unsigned short a)
{
 int loop;
  for (loop = a - 1; loop >= 0; loop--)
   key_put(keyring->keys[loop]);
}
