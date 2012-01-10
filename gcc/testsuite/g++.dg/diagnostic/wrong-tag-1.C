// Origin PR c++/51427

typedef struct _GMutex GMutex; // { dg-message "previously declared here"}
typedef union _GMutex GMutex; // { dg-error "tag used in naming" }
