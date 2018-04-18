/* PR c++/23180.  */
/* Initialize a local structure with an expression that attempts to use
   pointer arithmetic to calculate another structure field offset.  */

void saveLoadEntries(const void *);

void saveOrLoad() {
    struct Track {
        char soundName[15];
    };
    struct SaveLoadEntry {
        int offs;
        int type;
        int size;
    };    

    SaveLoadEntry trackEntries = {
	((int) (__SIZE_TYPE__) (&((Track *) 42)->soundName[0])) - 42,
        0, 1
    };
    saveLoadEntries(&trackEntries);
}
