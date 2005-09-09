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
	((long) (&((Track *) 42)->soundName[0])) - 42,
        0, 1
    };
    saveLoadEntries(&trackEntries);
}
