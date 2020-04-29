#ifndef RUST_OBJECT_EXPORT_H
#define RUST_OBJECT_EXPORT_H

extern unsigned int rust_field_alignment (tree t);

extern const char *
rust_read_export_data (int fd, off_t offset, char **pbuf, size_t *plen,
                       int *perr);
extern void
rust_write_export_data (const char *bytes, unsigned int size);

#endif // RUST_OBJECT_EXPORT_H
